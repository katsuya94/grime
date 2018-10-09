package eval

import (
	"fmt"

	"github.com/katsuya94/grime/common"
)

func CompileBody(env common.Environment, forms []common.Datum) (common.Expression, map[common.Symbol]common.Binding, error) {
	var (
		i                   int
		definitionNames     []common.Symbol
		definitionVariables []*common.Variable
		definitionForms     []common.Datum
	)
	// Expand and handle definitions, deferring expansion of variable definitions.
	for i = 0; i < len(forms); i++ {
		form := forms[i]
		processed := false
		expression := false
		for !processed {
			switch form := form.(type) {
			case common.DefineSyntaxForm:
				transformerEnv := env.Next()
				expression, err := Compile(transformerEnv, form.Form)
				if err != nil {
					return nil, nil, err
				}
				value, err := EvaluateExpressionOnce(expression)
				if err != nil {
					return nil, nil, err
				}
				procedure, ok := value.(common.Procedure)
				if !ok {
					return nil, nil, fmt.Errorf("expand: non-procedure as transformer")
				}
				env = env.Set(form.Name, common.Keyword{procedure})
				processed = true
			case common.DefineForm:
				variable := &common.Undeferred{}
				env = env.Set(form.Name, variable)
				definitionVariables = append(definitionVariables, variable)
				definitionForms = append(definitionForms, form.Form)
				processed = true
			case common.BeginForm:
				if len(forms) == i+1 {
					processed = true
					expression = true
					break
				}
				following := forms[i+1:]
				forms = forms[0:i]
				forms = append(forms, form.Forms...)
				forms = append(forms, following...)
				i--
				processed = true
			case common.LetSyntaxForm:
				return nil, nil, fmt.Errorf("expand: let-syntax not implemented")
			default:
				s, ok, err := Expand(env, form)
				if err != nil {
					return nil, nil, err
				} else if ok {
					form = s
				} else {
					processed = true
					expression = true
				}
			}
		}
		if expression {
			break
		}
	}
	// Compile set! expressions for the definitions.
	var expressions []common.Expression
	for i := range definitionVariables {
		expression, err := Compile(env, definitionForms[i])
		if err != nil {
			return nil, nil, err
		}
		expressions = append(expressions, common.Set{definitionVariables[i], expression})
	}
	// Create a begin form with the remaining expressions.
	for _, form := range forms[i:] {
		expression, err := Compile(env, form)
		if err != nil {
			return nil, nil, err
		}
		expressions = append(expressions, expression)
	}
	if len(expressions) == 0 {
		return nil, nil, fmt.Errorf("compile: begin: empty in expression context")
	}
	var expression common.Expression
	if len(expressions) > 1 {
		expression = expressions[0]
	} else {
		expression = common.Begin{expressions}
	}
	return expression, env.Bindings(), nil
}

func Compile(env common.Environment, form common.Datum) (common.Expression, error) {
	form, err := ExpandCompletely(env, form)
	if err != nil {
		return nil, err
	}
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
	case common.SyntaxForm:
		return common.Syntax{form.Datum}, nil
	case common.BeginForm:
		expression, _, err := CompileBody(env, form.Forms)
		if err != nil {
			return nil, err
		}
		return expression, nil
	case common.IfForm:
		conditionExpression, err := Compile(env, form.Condition)
		if err != nil {
			return nil, err
		}
		thenExpression, err := Compile(env, form.Then)
		if err != nil {
			return nil, err
		}
		elseExpression, err := Compile(env, form.Else)
		if err != nil {
			return nil, err
		}
		return common.If{conditionExpression, thenExpression, elseExpression}, nil
	case common.LetForm:
		initExpression, err := Compile(env, form.Init)
		if err != nil {
			return nil, err
		}
		variable := &common.Variable{common.Void}
		env = env.Set(form.Name, variable)
		bodyExpression, err := Compile(env, form.Body)
		if err != nil {
			return nil, err
		}
		return common.Let{variable, initExpression, bodyExpression}, nil
	case common.ApplicationForm:
		procedureExpression, err := Compile(env, form.Procedure)
		if err != nil {
			return nil, err
		}
		var argumentExpressions []common.Expression
		for _, argumentForm := range form.Arguments {
			expression, err := Compile(env, argumentForm)
			if err != nil {
				return nil, err
			}
			argumentExpressions = append(argumentExpressions, expression)
		}
		return common.Application{procedureExpression, argumentExpressions}, nil
	case common.LambdaForm:
		var variables []*common.Variable
		for _, formal := range form.Formals {
			variable := &common.Variable{common.Void}
			env = env.Set(formal, variable)
			variables = append(variables, variable)
		}
		expression, err := Compile(env, form.Body)
		if err != nil {
			return nil, err
		}
		return common.Lambda{variables, expression}, nil
	case common.ReferenceForm:
		binding := env.Get(form.Name)
		if binding == nil {
			return nil, fmt.Errorf("compile: unbound identifier %v", form.Name)
		}
		variable, ok := binding.(*common.Variable)
		if !ok {
			return nil, fmt.Errorf("compile: non-variable identifier %v in expression context", form.Name)
		}
		return common.Reference{variable}, nil
	case common.SetForm:
		binding := env.Get(form.Name)
		if binding == nil {
			return nil, fmt.Errorf("compile: unbound identifier %v", form.Name)
		}
		variable, ok := binding.(*common.Variable)
		if !ok {
			return nil, fmt.Errorf("compile: non-variable identifier %v in set!", form.Name)
		}
		expression, err := Compile(env, form.Form)
		if err != nil {
			return nil, err
		}
		return common.Set{variable, expression}, nil
	default:
		return nil, fmt.Errorf("compile: unhandled form %#v", form)
	}
}
