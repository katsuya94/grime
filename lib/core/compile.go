package core

import (
	"fmt"

	"github.com/katsuya94/grime/common"
)

func Compile(body common.WrappedSyntax) (common.Expression, map[common.Symbol]common.Location, error) {
	definitions := body.Definitions()
	forms, err := util.Slice(body.Datum())
	if err != nil {
		return nil, nil, err
	}
	return compileBody(definitions, forms, 0)
}

func compileBody(definitions map[common.Symbol]common.Location, forms []common.Datum, phase int) (common.Expression, map[common.Symbol]common.Location, error) {
	var (
		i                   int
		definitionVariables []*common.Variable
		definitionForms     []common.Datum
	)
	// Expand and handle definitions, deferring expansion of variable definitions.
	for i = 0; i < len(forms); i++ {
		processed := false
		expression := false
		for !processed {
			form := forms[i]
			switch v := form.(type) {
			case DefineSyntaxForm:
				_, ok := definitions[name]
				if ok {
					return fmt.Errorf("compile: %v: already defined")
				}
				name, _ := v.Identifier.IdentifierAt(phase)
				keyword := &common.Keyword{common.Void} // TODO need to implement cannot expand transformer before definition
				form := syntaxSet(v.Form, name, keyword)
				expression, err := compile(form, level+1)
				if err != nil {
					return nil, nil, err
				}
				value, err := common.EvaluateOnce(expression)
				if err != nil {
					return nil, nil, err
				}
				procedure, ok := value.(common.Procedure)
				if !ok {
					return nil, nil, fmt.Errorf("compile: define-syntax: expected procedure")
				}
				keyword.Transformer = procedure
				env, err = env.Define(v.Identifier, nil, &common.Keyword{procedure})
				if err != nil {
					return nil, nil, err
				}
				processed = true
			case DefineForm:
				variable := &common.Variable{common.Void, false}
				definitionVariables = append(definitionVariables, variable)
				definitionForms = append(definitionForms, v.Form)
				var err error
				env, err = env.Define(v.Name, nil, variable)
				if err != nil {
					return nil, nil, err
				}
				processed = true
			case BeginForm:
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
			case LetSyntaxForm:
				return nil, nil, fmt.Errorf("compile: let-syntax not implemented")
			default:
				if syntax, ok := v.(common.WrappedSyntax); ok {
					v, ok, err := expand(env, syntax)
					if err != nil {
						return nil, nil, err
					} else if ok {
						forms[i] = v
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
		expression, err := compile(env.Clear(), definitionForms[i])
		if err != nil {
			return nil, nil, err
		}
		expressions = append(expressions, Define{definitionVariables[i], expression})
	}
	// Compile the remaining expressions.
	if len(forms[i:]) == 0 {
		return nil, nil, common.ErrUnexpectedFinalForm
	}
	for _, form := range forms[i:] {
		expression, err := compile(env.Clear(), form)
		if err != nil {
			return nil, nil, err
		}
		expressions = append(expressions, expression)
	}
	var expression common.Expression
	if len(expressions) > 1 {
		expression = Begin{expressions}
	} else {
		expression = expressions[0]
	}
	return expression, env.Bindings(), nil
}

func compile(env common.Environment, form common.Datum) (common.Expression, error) {
	var err error
	form, err = expandCompletely(env, form)
	if err != nil {
		return nil, err
	}
	switch form := form.(type) {
	case QuoteForm:
		return Literal{form.Datum}, nil
	case SyntaxForm:
		template, patternVariablesUnexpanded, err := compileTemplate(env, form.Datum)
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
		return SyntaxTemplate{template, patternVariables}, nil
	case BeginForm:
		expression, _, err := compileBody(env, form.Forms)
		if err != nil {
			return nil, err
		}
		return expression, nil
	case IfForm:
		conditionExpression, err := compile(env, form.Condition)
		if err != nil {
			return nil, err
		}
		thenExpression, err := compile(env, form.Then)
		if err != nil {
			return nil, err
		}
		elseExpression, err := compile(env, form.Else)
		if err != nil {
			return nil, err
		}
		return If{conditionExpression, thenExpression, elseExpression}, nil
	case LetForm:
		initExpression, err := compile(env, form.Init)
		if err != nil {
			return nil, err
		}
		variable := &common.Variable{common.Void, true}
		name, location := form.Identifier.IdentifierAt(level)
		body := syntaxSet(form.Body, name, location)
		bodyExpression, err := compile(env, body)
		if err != nil {
			return nil, err
		}
		return Let{variable, initExpression, bodyExpression}, nil
	case ApplicationForm:
		procedureExpression, err := compile(env, form.Procedure)
		if err != nil {
			return nil, err
		}
		var argumentExpressions []common.Expression
		for _, argumentForm := range form.Arguments {
			expression, err := compile(env, argumentForm)
			if err != nil {
				return nil, err
			}
			argumentExpressions = append(argumentExpressions, expression)
		}
		return Application{procedureExpression, argumentExpressions}, nil
	case LambdaForm:
		var variables []*common.Variable
		body := form.Body
		for _, formal := range form.Formals {
			name, _ := formal.IdentifierAt(level)
			variable := &common.Variable{common.Void, true}
			body = syntaxSet(body, name, variable)
			variables = append(variables, variable)
		}
		expression, err := compile(env, body)
		if err != nil {
			return nil, err
		}
		return Literal{common.Lambda{variables, expression}}, nil
	case ReferenceForm:
		name, location := form.Identifier.IdentifierAt(level)
		if location == nil {
			return nil, fmt.Errorf("compile: unbound identifier %v", name)
		}
		variable, ok := location.(*common.Variable)
		if !ok {
			return nil, fmt.Errorf("compile: non-variable identifier %v in expression context", name)
		}
		return Reference{variable}, nil
	case SetForm:
		name, location := form.Identifier.IdentifierAt(level)
		if location == nil {
			return nil, fmt.Errorf("compile: unbound identifier %v", name)
		}
		variable, ok := location.(*common.Variable)
		if !ok {
			return nil, fmt.Errorf("compile: non-variable identifier %v in set!", name)
		}
		expression, err := compile(env, form.Form)
		if err != nil {
			return nil, err
		}
		return Set{variable, expression}, nil
	case SyntaxCaseForm:
		inputExpression, err := compile(env, form.Input)
		if err != nil {
			return nil, err
		}
		literals := make(map[common.Symbol]common.Location)
		for _, literal := range form.Literals {
			name, location := literal.IdentifierAt(level)
			if location == underscoreKeyword {
				return nil, fmt.Errorf("compile: underscore cannot appear in literals")
			}
			if location == ellipsisKeyword {
				return nil, fmt.Errorf("compile: ellipsis cannot appear in literals")
			}
			literals[name] = location
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
			patternVariables, err := common.PatternVariables(pattern, literals)
			if err != nil {
				return nil, err
			}
			bindings := make(map[common.Symbol]*common.PatternVariable)
			fender := form.Fenders[i]
			output := form.Outputs[i]
			for name, n := range patternVariables {
				patternVariable := &common.PatternVariable{common.NewWrappedSyntax(common.Void), n}
				bindings[name] = patternVariable
				fender = syntaxSet(fender, name, patternVariable)
				output = syntaxSet(output, name, patternVariable)
			}
			fenderExpression, err := compile(env, fender)
			if err != nil {
				return nil, err
			}
			outputExpression, err := compile(env, output)
			if err != nil {
				return nil, err
			}
			patterns = append(patterns, pattern)
			patternVariableBindings = append(patternVariableBindings, bindings)
			fenderExpressions = append(fenderExpressions, fenderExpression)
			outputExpressions = append(outputExpressions, outputExpression)
		}
		return SyntaxCase{inputExpression, literals, patterns, patternVariableBindings, fenderExpressions, outputExpressions}, nil
	case DefineForm, DefineSyntaxForm:
		return nil, fmt.Errorf("compile: unexpected body form in expression context")
	case common.WrappedSyntax:
		switch datum := form.Datum().(type) {
		case common.Boolean, common.Number, common.Character, common.String:
			return Literal{datum}, nil
		default:
			switch form.Datum() {
			case common.Null, common.Void:
				return Literal{datum}, nil
			default:
				return nil, fmt.Errorf("compile: unhandled literal %#v", datum)
			}
		}
	default:
		return nil, fmt.Errorf("compile: unhandled form %#v", form)
	}
}

// syntaxSet sets a substitution for on the given form if the given form is a wrapped syntax object.
func syntaxSet(form common.Datum, name common.Symbol, location common.Location) common.Datum {
	syntax, ok := form.(common.WrappedSyntax)
	if ok {
		form = syntax.Set(name, location)
	}
	return form
}

func compileTemplate(env common.Environment, datum common.Datum) (common.Datum, map[*common.PatternVariable]int, error) {
	syntax, isSyntax := datum.(common.WrappedSyntax)
	if isSyntax {
		datum = syntax.Datum()
	}
	switch datum := datum.(type) {
	case common.Boolean, common.Number, common.Character, common.String:
		return syntax, map[*common.PatternVariable]int{}, nil
	case common.Symbol:
		if !isSyntax {
			return nil, nil, fmt.Errorf("compile: encountered unwrapped symbol in syntax template")
		}
		binding := env.Get(datum)
		if patternVariable, ok := binding.(*common.PatternVariable); ok {
			return PatternVariableReference{patternVariable}, map[*common.PatternVariable]int{patternVariable: patternVariable.Nesting}, nil
		}
		if binding == ellipsisKeyword {
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
			if env.Get(name) != ellipsisKeyword {
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
			firstCompiled = Subtemplate{SyntaxTemplate{firstCompiled, patternVariables}, ellipsis, expansionPatternVariables}
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
		if datum == common.Null {
			return syntax, map[*common.PatternVariable]int{}, nil
		}
		return nil, nil, fmt.Errorf("compile: unhandled syntax template form %#v", datum)
	}
}

func compilePattern(env common.Environment, datum common.Datum) (common.Datum, error) {
	syntax, isSyntax := datum.(common.WrappedSyntax)
	if isSyntax {
		datum = syntax.Datum()
	}
	switch datum := datum.(type) {
	case common.Boolean, common.Number, common.Character, common.String:
		return datum, nil
	case common.Symbol:
		binding := env.Get(datum)
		if binding == underscoreKeyword {
			return common.Underscore, nil
		}
		if binding == ellipsisKeyword {
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
		if datum == common.Null {
			return common.Null, nil
		}
		return nil, fmt.Errorf("compile: unhandled pattern %#v", datum)
	}
}
