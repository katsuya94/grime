package eval

import (
	"fmt"

	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/util"
)

func CompileBody(env common.Environment, forms []common.Form) (common.Expression, common.BindingSet, error) {
	var (
		i                   int
		definitionVariables []*common.Variable
		definitionForms     []common.Datum
	)
	// Expand and handle definitions, deferring expansion of variable definitions.
	for i = 0; i < len(forms); i++ {
		form := forms[i]
		processed := false
		expression := false
		for !processed {
			switch v := form.(type) {
			case common.DefineSyntaxForm:
				expression, err := Compile(env.Next().Clear(), v.Form)
				if err != nil {
					return nil, nil, err
				}
				value, err := EvaluateExpressionOnce(expression)
				if err != nil {
					return nil, nil, err
				}
				procedure, ok := value.(common.Procedure)
				if !ok {
					return nil, nil, fmt.Errorf("compile: define-syntax: expected procedure")
				}
				env, err = env.Define(v.Name, nil, &common.Keyword{procedure})
				if err != nil {
					return nil, nil, err
				}
				processed = true
			case common.DefineForm:
				variable := &common.Variable{common.Void, false}
				definitionVariables = append(definitionVariables, variable)
				definitionForms = append(definitionForms, v.Form)
				var err error
				env, err = env.Define(v.Name, nil, variable)
				if err != nil {
					return nil, nil, err
				}
				processed = true
			case common.BeginForm:
				if len(forms) == i+1 {
					processed = true
					expression = true
					break
				}
				following := forms[i+1:]
				forms = forms[0:i]
				forms = append(forms, v.Forms...)
				forms = append(forms, following...)
				i--
				processed = true
			case common.LetSyntaxForm:
				return nil, nil, fmt.Errorf("compile: let-syntax not implemented")
			default:
				if syntax, ok := v.(common.WrappedSyntax); ok {
					v, ok, err := Expand(env, syntax)
					if err != nil {
						return nil, nil, err
					} else if ok {
						form = v // TODO: try forms[i] = v to prevent repeated expands
						continue
					}
				}
				processed = true
				expression = true
			}
		}
		if expression {
			break
		}
	}
	// Compile define expressions for the definitions.
	var expressions []common.Expression
	for i := range definitionVariables {
		expression, err := Compile(env.Clear(), definitionForms[i])
		if err != nil {
			return nil, nil, err
		}
		expressions = append(expressions, common.Define{definitionVariables[i], expression})
	}
	// Compile the remaining expressions.
	if len(forms[i:]) == 0 {
		return nil, nil, fmt.Errorf("compile: no expressions in body")
	}
	for _, form := range forms[i:] {
		expression, err := Compile(env.Clear(), form)
		if err != nil {
			return nil, nil, err
		}
		expressions = append(expressions, expression)
	}
	var expression common.Expression
	if len(expressions) > 1 {
		expression = common.Begin{expressions}
	} else {
		expression = expressions[0]
	}
	return expression, env.Bindings(), nil
}

func Compile(env common.Environment, form common.Form) (common.Expression, error) {
	var err error
	form, err = ExpandCompletely(env, form)
	if err != nil {
		return nil, err
	}
	switch form := form.(type) {
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
		template, patternVariablesUnexpanded, err := compileTemplate(env, form.Syntax)
		if err != nil {
			return nil, err
		}
		var patternVariables []*common.PatternVariable
		for patternVariable, n := range patternVariablesUnexpanded {
			if n > 0 {
				return nil, fmt.Errorf("compile: pattern variable not fully expanded")
			}
			patternVariables = append(patternVariables, patternVariable)
		}
		return common.SyntaxTemplate{template, patternVariables}, nil
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
		variable := &common.Variable{common.Void, true}
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
			variable := &common.Variable{common.Void, true}
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
	case common.SyntaxCaseForm:
		inputExpression, err := Compile(env, form.Input)
		if err != nil {
			return nil, err
		}
		literals := make(map[common.Symbol]common.Location)
		for _, literal := range form.Literals {
			binding := env.Get(literal)
			if binding == common.WildcardKeyword {
				return nil, fmt.Errorf("compile: wildcard cannot appear in literals")
			}
			if binding == common.EllipsisKeyword {
				return nil, fmt.Errorf("compile: ellipsis cannot appear in literals")
			}
			literals[literal] = binding
		}
		var (
			patterns                []common.Datum
			patternVariableBindings []map[common.Symbol]*common.PatternVariable
			fenderExpressions       []common.Expression
			outputExpressions       []common.Expression
		)
		for i := range form.Patterns {
			pattern, err := compilePattern(env, form.Patterns[i])
			if err != nil {
				return nil, err
			}
			patternVariables, err := util.PatternVariables(pattern, literals)
			if err != nil {
				return nil, err
			}
			bindings := make(map[common.Symbol]*common.PatternVariable)
			clauseEnv := env
			for name, n := range patternVariables {
				patternVariable := &common.PatternVariable{common.NewWrappedSyntax(common.Void), n}
				bindings[name] = patternVariable
				clauseEnv = clauseEnv.Set(name, patternVariable)
			}
			fenderExpression, err := Compile(clauseEnv, form.Fenders[i])
			if err != nil {
				return nil, err
			}
			outputExpression, err := Compile(clauseEnv, form.Outputs[i])
			if err != nil {
				return nil, err
			}
			patterns = append(patterns, pattern)
			patternVariableBindings = append(patternVariableBindings, bindings)
			fenderExpressions = append(fenderExpressions, fenderExpression)
			outputExpressions = append(outputExpressions, outputExpression)
		}
		return common.SyntaxCase{inputExpression, literals, patterns, patternVariableBindings, fenderExpressions, outputExpressions}, nil
	case common.DefineForm, common.DefineSyntaxForm:
		return nil, fmt.Errorf("compile: unexpected body form in expression context")
	case common.WrappedSyntax:
		switch datum := form.Datum().(type) {
		case common.Boolean, common.Number, common.Character, common.String, nil:
			return datum.(common.Expression), nil
		default:
			if datum == common.Void {
				return common.Void, nil
			}
			return nil, fmt.Errorf("compile: unhandled literal %#v", datum)
		}
	default:
		return nil, fmt.Errorf("compile: unhandled form %#v", form)
	}
}

func compileTemplate(env common.Environment, datum common.Datum) (common.Datum, map[*common.PatternVariable]int, error) {
	syntax, isSyntax := datum.(common.WrappedSyntax)
	if isSyntax {
		datum = syntax.Datum()
	}
	switch datum := datum.(type) {
	case common.Boolean, common.Number, common.Character, common.String, nil:
		return syntax, map[*common.PatternVariable]int{}, nil
	case common.Symbol:
		binding := env.Get(datum)
		if patternVariable, ok := binding.(*common.PatternVariable); ok {
			return common.PatternVariableReference{patternVariable}, map[*common.PatternVariable]int{patternVariable: patternVariable.Nesting}, nil
		}
		if binding == common.EllipsisKeyword {
			return nil, nil, fmt.Errorf("compile: malformed syntax template")
		}
		return syntax, map[*common.PatternVariable]int{}, nil
	case common.Pair:
		ellipsis := 0
		rest := datum.Rest
		for {
			pair, ok := rest.(common.Pair)
			if !ok {
				break
			}
			name, ok := pair.First.(common.Symbol)
			if !ok {
				break
			}
			if env.Get(name) != common.EllipsisKeyword {
				break
			}
			ellipsis++
			rest = pair.Rest
		}
		var (
			firstTemplate common.Datum
			restTemplate  common.Datum
		)
		if isSyntax {
			firstTemplate = syntax.PushOnto(datum.First)
			restTemplate = syntax.PushOnto(rest)
		} else {
			firstTemplate = datum.First
			restTemplate = datum.Rest
		}
		firstCompiled, firstPatternVariables, err := compileTemplate(env, firstTemplate)
		if err != nil {
			return nil, nil, err
		}
		_, firstStatic := firstCompiled.(common.WrappedSyntax)
		if firstStatic && ellipsis > 0 {
			return nil, nil, fmt.Errorf("compile: syntax subtemplate must contain a pattern variable")
		}
		restCompiled, restPatternVariables, err := compileTemplate(env, restTemplate)
		if err != nil {
			return nil, nil, err
		}
		_, restStatic := restCompiled.(common.WrappedSyntax)
		if firstStatic && restStatic {
			return syntax, map[*common.PatternVariable]int{}, nil
		}
		if ellipsis > 0 {
			var expansionPatternVariables []*common.PatternVariable
			var patternVariables []*common.PatternVariable
			for patternVariable, n := range firstPatternVariables {
				if n >= ellipsis {
					firstPatternVariables[patternVariable] -= ellipsis
					expansionPatternVariables = append(expansionPatternVariables, patternVariable)
				}
				patternVariables = append(patternVariables, patternVariable)
			}
			if len(expansionPatternVariables) == 0 {
				return nil, nil, fmt.Errorf("compile: syntax subtemplate must contain a pattern variable determining expansion count")
			}
			firstCompiled = common.Subtemplate{common.SyntaxTemplate{firstCompiled, patternVariables}, ellipsis, expansionPatternVariables}
		}
		patternVariables := make(map[*common.PatternVariable]int)
		for patternVariable, n := range firstPatternVariables {
			patternVariables[patternVariable] = n
		}
		for patternVariable, n := range restPatternVariables {
			if first, ok := patternVariables[patternVariable]; ok {
				if n != first {
					return nil, nil, fmt.Errorf("compile: nested expansion of a single pattern variable")
				}
			}
			patternVariables[patternVariable] = n
		}
		return common.Pair{firstCompiled, restCompiled}, patternVariables, nil
	default:
		return nil, nil, fmt.Errorf("compile: unhandled syntax template form %#v", datum)
	}
}

func compilePattern(env common.Environment, datum common.Datum) (common.Datum, error) {
	syntax, isSyntax := datum.(common.WrappedSyntax)
	if isSyntax {
		datum = syntax.Datum()
	}
	switch datum := datum.(type) {
	case common.Boolean, common.Number, common.Character, common.String, nil:
		return datum, nil
	case common.Symbol:
		binding := env.Get(datum)
		if binding == common.WildcardKeyword {
			return common.Wildcard, nil
		}
		if binding == common.EllipsisKeyword {
			return common.Ellipsis, nil
		}
		return datum, nil
	case common.Pair:
		var (
			firstPattern common.Datum
			restPattern  common.Datum
		)
		if isSyntax {
			firstPattern = syntax.PushOnto(datum.First)
			restPattern = syntax.PushOnto(datum.Rest)
		} else {
			firstPattern = datum.First
			restPattern = datum.Rest
		}
		first, err := compilePattern(env, firstPattern)
		if err != nil {
			return nil, err
		}
		rest, err := compilePattern(env, restPattern)
		if err != nil {
			return nil, err
		}
		return common.Pair{first, rest}, nil
	default:
		return nil, fmt.Errorf("compile: unhandled pattern %#v", datum)
	}
}
