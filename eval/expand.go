package eval

import (
	"fmt"

	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/read"
	"github.com/katsuya94/grime/util"
)

func ExpandBody(env common.Environment, forms []common.Datum) (common.Datum, error) {
	var (
		i               int
		definitionNames []common.Symbol
		definitionForms []common.Datum
	)
	// Expand and handle definitions, deferring expansion of variable definitions.
	for i = 0; i < len(forms); i++ {
		form, err := Expand(env, forms[i])
		if err != nil {
			return nil, err
		}
		switch v := form.(type) {
		case common.DefineSyntaxForm:
			return nil, fmt.Errorf("expand: define-syntax not implemented")
		case common.DefineForm:
			definitionNames = append(definitionNames, v.Name)
			definitionForms = append(definitionForms, v.Form)
			continue
		case common.BeginForm:
			if len(forms) == i+1 {
				break
			}
			following := forms[i+1:]
			forms = forms[0:i]
			for _, form := range v.Forms {
				forms = append(forms, form)
			}
			forms = append(forms, following...)
			i--
			continue
		case common.LetSyntaxForm:
			return nil, fmt.Errorf("expand: let-syntax not implemented")
		}
		break
	}
	// Create a begin form with the remaining expressions.
	if len(forms[i:]) == 0 {
		return nil, fmt.Errorf("expand: begin: empty in expression context")
	}
	var form common.Datum = common.BeginForm{forms[i:]}
	// Wrap it in a letrec* with the definitions.
	for i := len(definitionNames) - 1; i >= 0; i-- {
		form = common.LetForm{definitionNames[i], definitionForms[i], form}
	}
	return form, nil
}

var (
	PatternMacroUseList                = read.MustReadString("(keyword _ ...)")[0]
	PatternMacroUseImproperList        = read.MustReadString("(keyword _ ... . _)")[0]
	PatternMacroUseSingletonIdentifier = read.MustReadString("keyword")[0]
	PatternMacroUseSet                 = read.MustReadString("(set! keyword _)")[0]
	PatternApplication                 = read.MustReadString("(procedure arguments ...)")[0]
)

func Expand(env common.Environment, syntax common.Datum) (common.Datum, error) {
	// TODO use literals to ensure that set! would point at the keyword in base
	if form, ok, err := expandMacroMatching(env, syntax, PatternMacroUseSet); err != nil {
		return nil, err
	} else if ok {
		return form, nil
	}
	if form, ok, err := expandMacroMatching(env, syntax, PatternMacroUseList); err != nil {
		return nil, err
	} else if ok {
		return form, nil
	}
	if form, ok, err := expandMacroMatching(env, syntax, PatternMacroUseImproperList); err != nil {
		return nil, err
	} else if ok {
		return form, nil
	}
	if form, ok, err := expandMacroMatching(env, syntax, PatternMacroUseSingletonIdentifier); err != nil {
		return nil, err
	} else if ok {
		return form, nil
	}
	if result, ok, err := util.Match(syntax, PatternApplication, nil); err != nil {
		return nil, err
	} else if ok {
		procedure, ok := result[common.Symbol("procedure")].(common.Datum)
		if !ok {
			return nil, fmt.Errorf("application: bad syntax")
		}
		var arguments []common.Datum
		for _, argument := range result[common.Symbol("arguments")].([]interface{}) {
			if argument, ok := argument.(common.Datum); !ok {
				return nil, fmt.Errorf("application: bad syntax")
			} else {
				arguments = append(arguments, argument)
			}
		}
		return common.ApplicationForm{procedure, arguments}, nil
	}
	if name, ok := syntax.(common.Symbol); ok {
		return common.ReferenceForm{name}, nil
	}
	return syntax, nil
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
		return Apply(escape, keyword.Transformer, syntax)
	})
	if err != nil {
		return nil, false, err
	}
	return output, true, nil
}
