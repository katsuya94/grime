package core

import (
	"fmt"

	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/read"
)

var (
	PatternMacroUseList                = common.Pattern(read.MustReadDatum("(keyword _ ...)"))
	PatternMacroUseImproperList        = common.Pattern(read.MustReadDatum("(keyword _ ... . _)"))
	PatternMacroUseSingletonIdentifier = common.Pattern(read.MustReadDatum("keyword"))
	PatternMacroUseSet                 = common.Pattern(read.MustReadDatum("(set! keyword _)"))
	PatternApplication                 = common.Pattern(read.MustReadDatum("(procedure arguments ...)"))
)

func Expand(compiler Compiler, form common.Datum) (common.Datum, bool, error) {
	if form, ok, err := expandMacroMatching(form, compiler.Phase, PatternMacroUseSet, map[common.Symbol]common.Location{
		common.Symbol("set!"): setKeyword,
	}); ok || err != nil {
		return form, ok, err
	}
	if form, ok, err := expandMacroMatching(form, compiler.Phase, PatternMacroUseList, nil); ok || err != nil {
		return form, ok, err
	}
	if form, ok, err := expandMacroMatching(form, compiler.Phase, PatternMacroUseImproperList, nil); ok || err != nil {
		return form, ok, err
	}
	if form, ok, err := expandMacroMatching(form, compiler.Phase, PatternMacroUseSingletonIdentifier, nil); ok || err != nil {
		return form, ok, err
	}
	if result, ok, err := common.MatchSyntax(form, PatternApplication, nil); err != nil {
		return nil, false, err
	} else if ok {
		procedure := result[common.Symbol("procedure")]
		var arguments []common.Datum
		for _, argument := range result[common.Symbol("arguments")].([]interface{}) {
			arguments = append(arguments, argument)
		}
		return ApplicationForm{procedure, arguments}, true, nil
	}
	id, ok := common.Syntax{form}.Identifier()
	if ok {
		return ReferenceForm{id}, true, nil
	}
	return nil, false, nil
}

func expandMacroMatching(form common.Datum, phase int, pattern common.Datum, literals map[common.Symbol]common.Location) (common.Datum, bool, error) {
	result, ok, err := common.MatchSyntax(form, pattern, literals)
	if err != nil {
		return nil, false, err
	} else if !ok {
		return nil, false, nil
	}
	id, ok := common.Syntax{result[common.Symbol("keyword")]}.Identifier()
	if !ok {
		return nil, false, nil
	}
	location := id.Location()
	if location == nil {
		return nil, false, nil
	}
	keyword, ok := location.(*common.Keyword)
	if !ok {
		return nil, false, nil
	}
	if keyword.Transformer == nil {
		return nil, false, fmt.Errorf("expand: cannot use keyword before definition")
	}
	output, err := common.WithEscape(func(escape common.Continuation) (common.EvaluationResult, error) {
		return keyword.Transformer.Call(escape, form)
	})
	if err != nil {
		return nil, false, err
	}
	return output, true, nil
}
