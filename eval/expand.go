package eval

import (
	"fmt"

	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/read"
	"github.com/katsuya94/grime/util"
)

var (
	PatternMacroUseList                = read.MustReadString("(keyword _ ...)")[0]
	PatternMacroUseImproperList        = read.MustReadString("(keyword _ ... . _)")[0]
	PatternMacroUseSingletonIdentifier = read.MustReadString("keyword")[0]
	PatternMacroUseSet                 = read.MustReadString("(set! keyword _)")[0]
	PatternApplication                 = read.MustReadString("(procedure arguments ...)")[0]
)

func Expand(env common.Environment, syntax common.Datum) (common.Datum, bool, error) {
	// TODO use literals to ensure that set! would point at the keyword in base
	if form, ok, err := expandMacroMatching(env, syntax, PatternMacroUseSet); ok || err != nil {
		return form, ok, err
	}
	if form, ok, err := expandMacroMatching(env, syntax, PatternMacroUseList); ok || err != nil {
		return form, ok, err
	}
	if form, ok, err := expandMacroMatching(env, syntax, PatternMacroUseImproperList); ok || err != nil {
		return form, ok, err
	}
	if form, ok, err := expandMacroMatching(env, syntax, PatternMacroUseSingletonIdentifier); ok || err != nil {
		return form, ok, err
	}
	if result, ok, err := util.Match(syntax, PatternApplication, nil); err != nil {
		return nil, false, err
	} else if ok {
		procedure, ok := result[common.Symbol("procedure")].(common.Datum)
		if !ok {
			return nil, false, fmt.Errorf("application: bad syntax")
		}
		var arguments []common.Datum
		for _, argument := range result[common.Symbol("arguments")].([]interface{}) {
			if argument, ok := argument.(common.Datum); !ok {
				return nil, false, fmt.Errorf("application: bad syntax")
			} else {
				arguments = append(arguments, argument)
			}
		}
		return common.ApplicationForm{procedure, arguments}, true, nil
	}
	if name, ok := syntax.(common.Symbol); ok {
		return common.ReferenceForm{name}, true, nil
	}
	return nil, false, nil
}

func expandMacroMatching(env common.Environment, syntax common.Datum, pattern common.Datum) (common.Datum, bool, error) {
	// TODO identifiers are actually wrapped
	result, ok, err := util.Match(syntax, pattern, nil)
	if err != nil {
		return nil, false, err
	} else if !ok {
		return nil, false, nil
	}
	name, ok := result[common.Symbol("keyword")].(common.Symbol)
	if !ok {
		return nil, false, nil
	}
	binding := env.Get(name)
	if binding == nil {
		return nil, false, nil
	}
	keyword, ok := binding.(common.Keyword)
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

func ExpandCompletely(env common.Environment, syntax common.Datum) (common.Datum, error) {
	for {
		s, ok, err := Expand(env, syntax)
		if err != nil {
			return nil, err
		} else if !ok {
			return syntax, nil
		}
		syntax = s
	}
}
