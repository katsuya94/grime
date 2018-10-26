package eval

import (
	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/read"
	"github.com/katsuya94/grime/util"
)

var (
	PatternMacroUseList                = util.Pattern(read.MustReadString("(keyword _ ...)")[0])
	PatternMacroUseImproperList        = util.Pattern(read.MustReadString("(keyword _ ... . _)")[0])
	PatternMacroUseSingletonIdentifier = util.Pattern(read.MustReadString("keyword")[0])
	PatternMacroUseSet                 = util.Pattern(read.MustReadString("(set! keyword _)")[0])
	PatternApplication                 = util.Pattern(read.MustReadString("(procedure arguments ...)")[0])
)

func Expand(env common.Environment, syntax common.WrappedSyntax) (common.Form, bool, error) {
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
	if result, ok, err := util.MatchSyntax(syntax, PatternApplication, nil); err != nil {
		return nil, false, err
	} else if ok {
		procedure := result[common.Symbol("procedure")].(common.WrappedSyntax).Datum()
		var arguments []common.Form
		for _, argument := range result[common.Symbol("arguments")].([]interface{}) {
			argument := argument.(common.WrappedSyntax).Datum
			arguments = append(arguments, argument)
		}
		return common.ApplicationForm{procedure, arguments}, true, nil
	}
	if name, _, ok := syntax.Identifier(); ok {
		return common.ReferenceForm{name}, true, nil
	}
	return nil, false, nil
}

func expandMacroMatching(env common.Environment, syntax common.WrappedSyntax, pattern common.Datum, literals map[common.Symbol]common.Location) (common.Form, bool, error) {
	// TODO identifiers are actually wrapped
	result, ok, err := util.MatchSyntax(syntax, pattern, literals)
	if err != nil {
		return nil, false, err
	} else if !ok {
		return nil, false, nil
	}
	name, _, ok := result[common.Symbol("keyword")].(common.WrappedSyntax).Identifier()
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

func ExpandCompletely(env common.Environment, syntax common.WrappedSyntax) (common.Form, error) {
	var form common.Form = syntax
	for {
		syntax, ok := form.(common.WrappedSyntax)
		if ok {
			f, ok, err := Expand(env, syntax)
			if err != nil {
				return nil, err
			} else if ok {
				form = f
				continue
			}
		}
		return form, nil
	}
}
