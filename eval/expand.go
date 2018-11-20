package eval

import (
	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/read"
)

var (
	PatternMacroUseList                = common.Pattern(read.MustReadString("(keyword _ ...)")[0])
	PatternMacroUseImproperList        = common.Pattern(read.MustReadString("(keyword _ ... . _)")[0])
	PatternMacroUseSingletonIdentifier = common.Pattern(read.MustReadString("keyword")[0])
	PatternMacroUseSet                 = common.Pattern(read.MustReadString("(set! keyword _)")[0])
	PatternApplication                 = common.Pattern(read.MustReadString("(procedure arguments ...)")[0])
)

func Expand(env common.Environment, syntax common.Datum) (common.Datum, bool, error) {
	// TODO use literals to ensure that set! would point at the keyword in base
	if form, ok, err := expandMacroMatching(env, syntax, PatternMacroUseSet, map[common.Symbol]common.Location{
		common.Symbol("set!"): nil,
	}); ok || err != nil {
		return form, ok, err
	}
	if form, ok, err := expandMacroMatching(env, syntax, PatternMacroUseList, nil); ok || err != nil {
		return form, ok, err
	}
	if form, ok, err := expandMacroMatching(env, syntax, PatternMacroUseImproperList, nil); ok || err != nil {
		return form, ok, err
	}
	if form, ok, err := expandMacroMatching(env, syntax, PatternMacroUseSingletonIdentifier, nil); ok || err != nil {
		return form, ok, err
	}
	if result, ok, err := common.MatchSyntax(syntax, PatternApplication, nil); err != nil {
		return nil, false, err
	} else if ok {
		procedure := result[common.Symbol("procedure")]
		var arguments []common.Datum
		for _, argument := range result[common.Symbol("arguments")].([]interface{}) {
			arguments = append(arguments, argument)
		}
		return common.ApplicationForm{procedure, arguments}, true, nil
	}
	if syntax, ok := syntax.(common.WrappedSyntax); ok {
		if name, _, ok := syntax.Identifier(); ok {
			return common.ReferenceForm{name}, true, nil
		}
	}
	return nil, false, nil
}

func expandMacroMatching(env common.Environment, syntax common.Datum, pattern common.Datum, literals map[common.Symbol]common.Location) (common.Datum, bool, error) {
	// TODO identifiers are actually wrapped
	result, ok, err := common.MatchSyntax(syntax, pattern, literals)
	if err != nil {
		return nil, false, err
	} else if !ok {
		return nil, false, nil
	}
	identifier, ok := result[common.Symbol("keyword")].(common.WrappedSyntax)
	if !ok {
		return nil, false, nil
	}
	name, _, ok := identifier.Identifier()
	if !ok {
		return nil, false, nil
	}
	binding := env.Get(name)
	if binding == nil {
		return nil, false, nil
	}
	keyword, ok := binding.(*common.Keyword)
	if !ok {
		return nil, false, nil
	}
	output, err := CallWithEscape(func(escape common.Continuation) (common.EvaluationResult, error) {
		return keyword.Transformer.Call(escape, syntax)
	})
	if err != nil {
		return nil, false, err
	}
	return output, true, nil
}

func ExpandCompletely(env common.Environment, d common.Datum) (common.Datum, error) {
	for {
		if !common.IsSyntax(d) {
			return d, nil
		}
		expanded, ok, err := Expand(env, d)
		if err != nil {
			return nil, err
		} else if !ok {
			return d, nil
		}
		d = expanded
	}
}
