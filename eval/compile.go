package eval

import (
	"fmt"

	"github.com/katsuya94/grime/common"
)

func Compile(env common.Environment, form common.Datum) (common.Expression, error) {
	switch form := form.(type) {
	case common.Boolean, common.Number, common.Character, common.String:
		return form.(common.Expression), nil
	case common.QuoteForm:
		if form.Datum == nil {
			return nil, nil
		}
		expression, ok := form.Datum.(common.Expression)
		if !ok {
			return nil, fmt.Errorf("compile: cannot quote %#v", form.Datum)
		}
		return expression, nil
	case common.BeginForm:
		expanded, err := ExpandBody(env, form.Forms)
		if err != nil {
			return nil, err
		} else if begin, ok := expanded.(common.BeginForm); ok {
			var subExpressions []common.Expression
			for _, subForm := range begin.Forms {
				expanded, err := Expand(env, subForm)
				if err != nil {
					return nil, err
				}
				expression, err := Compile(env, expanded)
				if err != nil {
					return nil, err
				}
				subExpressions = append(subExpressions, expression)
			}
			return common.Begin{subExpressions}, nil
		}
		expression, err := Compile(env, expanded)
		if err != nil {
			return nil, err
		}
		return expression, nil
	case common.IfForm:
		conditionExpanded, err := Expand(env, form.Condition)
		if err != nil {
			return nil, err
		}
		conditionExpression, err := Compile(env, conditionExpanded)
		if err != nil {
			return nil, err
		}
		thenExpanded, err := Expand(env, form.Then)
		if err != nil {
			return nil, err
		}
		thenExpression, err := Compile(env, thenExpanded)
		if err != nil {
			return nil, err
		}
		elseExpanded, err := Expand(env, form.Else)
		if err != nil {
			return nil, err
		}
		elseExpression, err := Compile(env, elseExpanded)
		if err != nil {
			return nil, err
		}
		return common.If{conditionExpression, thenExpression, elseExpression}, nil
	case common.LetForm:
		initExpanded, err := Expand(env, form.Init)
		if err != nil {
			return nil, err
		}
		initExpression, err := Compile(env, initExpanded)
		if err != nil {
			return nil, err
		}
		bodyExpanded, err := Expand(env, form.Body)
		if err != nil {
			return nil, err
		}
		bodyExpression, err := Compile(env, bodyExpanded)
		if err != nil {
			return nil, err
		}
		return common.Let{form.Name, initExpression, bodyExpression}, nil
	case common.ApplicationForm:
		procedureExpanded, err := Expand(env, form.Procedure)
		if err != nil {
			return nil, err
		}
		procedureExpression, err := Compile(env, procedureExpanded)
		if err != nil {
			return nil, err
		}
		var argumentExpressions []common.Expression
		for _, argumentForm := range form.Arguments {
			expanded, err := Expand(env, argumentForm)
			if err != nil {
				return nil, err
			}
			expression, err := Compile(env, expanded)
			if err != nil {
				return nil, err
			}
			argumentExpressions = append(argumentExpressions, expression)
		}
		return common.Application{procedureExpression, argumentExpressions}, nil
	case common.LambdaForm:
		expanded, err := ExpandBody(env, form.Body)
		if err != nil {
			return nil, err
		}
		expression, err := Compile(env, expanded)
		if err != nil {
			return nil, err
		}
		return common.Lambda{form.Formals, expression}, nil
	case common.ReferenceForm:
		return common.Reference{form.Name}, nil
	case common.SetForm:
		expanded, err := Expand(env, form.Form)
		if err != nil {
			return nil, err
		}
		expression, err := Compile(env, expanded)
		if err != nil {
			return nil, err
		}
		return common.Set{form.Name, expression}, nil
	case common.DefineForm, common.DefineSyntaxForm, common.LetSyntaxForm:
		return nil, fmt.Errorf("compile: unexpected body form in expression context")
	default:
		expanded, err := Expand(env, form)
		if err != nil {
			return nil, err
		}
		expression, err := Compile(env, expanded)
		if err != nil {
			return nil, err
		}
		return expression, nil
	}
}
