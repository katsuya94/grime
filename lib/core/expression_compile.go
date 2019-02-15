package core

import (
	"fmt"

	"github.com/katsuya94/grime/common"
)

func ExpressionCompile(compiler Compiler, form common.Datum) (common.Expression, error) {
	form, err := compiler.ExpandCompletely(form)
	if err != nil {
		return nil, err
	}
	switch form := form.(type) {
	case QuoteForm:
		return Literal{form.Datum}, nil
	case SyntaxForm:
		template, patternVariablesUnexpanded, err := compileTemplate(common.NewSyntax(form.Datum))
		if err != nil {
			return nil, err
		}
		var patternVariables []*common.PatternVariable
		for patternVariable, n := range patternVariablesUnexpanded {
			if n > 0 {
				return nil, fmt.Errorf("compile: in syntax template at %v: encountered unexpanded pattern variable", common.NewSyntax(form.Datum).SourceLocation())
			}
			patternVariables = append(patternVariables, patternVariable)
		}
		return SyntaxTemplate{template, patternVariables}, nil
	case BeginForm:
		scope := common.NewScope()
		forms := make([]common.Datum, len(form.Forms))
		for i := range form.Forms {
			forms[i] = common.NewSyntax(form.Forms[i]).Push(scope, common.LEXICAL).Form()
		}
		expression, err := compiler.BodyCompile(forms, scope)
		if err != nil {
			return nil, err
		}
		return expression, nil
	case IfForm:
		conditionExpression, err := compiler.ExpressionCompile(form.Condition)
		if err != nil {
			return nil, err
		}
		thenExpression, err := compiler.ExpressionCompile(form.Then)
		if err != nil {
			return nil, err
		}
		elseExpression, err := compiler.ExpressionCompile(form.Else)
		if err != nil {
			return nil, err
		}
		return If{conditionExpression, thenExpression, elseExpression}, nil
	case LetForm:
		initExpression, err := compiler.ExpressionCompile(form.Init)
		if err != nil {
			return nil, err
		}
		variable := &common.Variable{}
		scope := common.NewScope()
		err = scope.Set(form.Identifier, variable)
		if err != nil {
			return nil, err
		}
		forms := make([]common.Datum, len(form.Body))
		for i := range form.Body {
			forms[i] = common.NewSyntax(form.Body[i]).Push(scope, common.LEXICAL).Form()
		}
		bodyExpression, err := compiler.BodyCompile(forms, scope)
		if err != nil {
			return nil, err
		}
		return Let{variable, initExpression, bodyExpression}, nil
	case ApplicationForm:
		procedureExpression, err := compiler.ExpressionCompile(form.Procedure)
		if err != nil {
			return nil, err
		}
		var argumentExpressions []common.Expression
		for _, argumentForm := range form.Arguments {
			expression, err := compiler.ExpressionCompile(argumentForm)
			if err != nil {
				return nil, err
			}
			argumentExpressions = append(argumentExpressions, expression)
		}
		return Application{procedureExpression, argumentExpressions}, nil
	case LambdaForm:
		var variables []*common.Variable
		scope := common.NewScope()
		for _, formal := range form.Formals {
			variable := &common.Variable{}
			err := scope.Set(formal, variable)
			if err != nil {
				return nil, err
			}
			variables = append(variables, variable)
		}
		forms := make([]common.Datum, len(form.Body))
		for i := range form.Body {
			forms[i] = common.NewSyntax(form.Body[i]).Push(scope, common.LEXICAL).Form()
		}
		expression, err := compiler.BodyCompile(forms, scope)
		if err != nil {
			return nil, err
		}
		return Literal{common.Lambda{variables, expression}}, nil
	case ReferenceForm:
		location := form.Identifier.Location()
		if location == nil {
			return nil, fmt.Errorf("compile: unbound identifier %v at %v", form.Identifier.Name(), form.Identifier.SourceLocation())
		}
		variable, ok := location.(*common.Variable)
		if !ok {
			return nil, fmt.Errorf("compile: non-variable identifier %v in expression context", form.Identifier.Name())
		}
		return Reference{form.Identifier, variable}, nil
	case SetForm:
		location := form.Identifier.Location()
		if location == nil {
			return nil, fmt.Errorf("compile: unbound identifier %v at %v", form.Identifier.Name(), form.Identifier.SourceLocation())
		}
		variable, ok := location.(*common.Variable)
		if !ok {
			return nil, fmt.Errorf("compile: non-variable identifier %v in assignment", form.Identifier.Name())
		}
		expression, err := compiler.ExpressionCompile(form.Form)
		if err != nil {
			return nil, err
		}
		return Set{form.Identifier, variable, expression}, nil
	case SyntaxCaseForm:
		inputExpression, err := compiler.ExpressionCompile(form.Input)
		if err != nil {
			return nil, err
		}
		literals := map[common.Symbol]common.Location{}
		for _, literal := range form.Literals {
			location := literal.Location()
			if location == underscoreKeyword {
				return nil, fmt.Errorf("compile: underscore cannot appear in literals")
			}
			if location == ellipsisKeyword {
				return nil, fmt.Errorf("compile: ellipsis cannot appear in literals")
			}
			literals[literal.Name()] = location
		}
		var (
			patterns                []common.Datum
			patternVariableBindings []map[common.Symbol]*common.PatternVariable
			fenderExpressions       []common.Expression
			outputExpressions       []common.Expression
		)
		for i := range form.Patterns {
			pattern, err := compilePattern(common.NewSyntax(form.Patterns[i]))
			if err != nil {
				return nil, err
			}
			patternVariables, err := common.PatternVariables(pattern, literals)
			if err != nil {
				return nil, err
			}
			bindings := map[common.Symbol]*common.PatternVariable{}
			scope := common.NewScope()
			for name, n := range patternVariables {
				patternVariable := &common.PatternVariable{nil, n}
				bindings[name] = patternVariable
				err := scope.Set(common.NewIdentifier(name), patternVariable)
				if err != nil {
					return nil, err
				}
			}
			fender := common.NewSyntax(form.Fenders[i]).Push(scope, common.LEXICAL).Form()
			output := common.NewSyntax(form.Outputs[i]).Push(scope, common.LEXICAL).Form()
			fenderExpression, err := compiler.ExpressionCompile(fender)
			if err != nil {
				return nil, err
			}
			outputExpression, err := compiler.ExpressionCompile(output)
			if err != nil {
				return nil, err
			}
			patterns = append(patterns, pattern)
			patternVariableBindings = append(patternVariableBindings, bindings)
			fenderExpressions = append(fenderExpressions, fenderExpression)
			outputExpressions = append(outputExpressions, outputExpression)
		}
		return SyntaxCase{inputExpression, literals, patterns, patternVariableBindings, fenderExpressions, outputExpressions}, nil
	case common.WrappedSyntax:
		switch datum := form.Datum().(type) {
		case common.Boolean, common.Number, common.Character, common.String:
			return Literal{datum}, nil
		default:
			switch form.Datum() {
			case common.Null, common.Void:
				return Literal{datum}, nil
			default:
				return nil, fmt.Errorf("compile: unhandled literal %v", common.Write(datum))
			}
		}
	default:
		return nil, fmt.Errorf("compile: unexpected form in expression context: %v %#v", common.Write(form), form)
	}
}

func compileTemplate(syntax common.Syntax) (common.Datum, map[*common.PatternVariable]int, error) {
	if id, ok := syntax.Identifier(); ok {
		location := id.Location()
		if patternVariable, ok := location.(*common.PatternVariable); ok {
			return PatternVariableReference{patternVariable}, map[*common.PatternVariable]int{patternVariable: patternVariable.Nesting}, nil
		}
		if location == ellipsisKeyword {
			return nil, nil, fmt.Errorf("compile: in syntax template at %v: improper use of ellipsis", id.SourceLocation())
		}
		return syntax.Form(), map[*common.PatternVariable]int{}, nil
	}
	if pair, ok := syntax.Pair(); ok {
		first := common.NewSyntax(pair.First)
		rest := common.NewSyntax(pair.Rest)
		ellipsis := 0
		for {
			pair, ok := rest.Pair()
			if !ok {
				break
			}
			id, ok := common.NewSyntax(pair.First).Identifier()
			if !ok {
				break
			}
			if id.Location() != ellipsisKeyword {
				break
			}
			ellipsis++
			rest = common.NewSyntax(pair.Rest)
		}
		firstCompiled, firstPatternVariables, err := compileTemplate(first)
		if err != nil {
			return nil, nil, err
		}
		firstStatic := common.IsSyntax(firstCompiled)
		if firstStatic && ellipsis > 0 {
			return nil, nil, fmt.Errorf("compile: in syntax template at %v: syntax subtemplate must contain a pattern variable", first.SourceLocation())
		}
		restCompiled, restPatternVariables, err := compileTemplate(rest)
		if err != nil {
			return nil, nil, err
		}
		restStatic := common.IsSyntax(restCompiled)
		if firstStatic && restStatic {
			return syntax.Form(), map[*common.PatternVariable]int{}, nil
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
				return nil, nil, fmt.Errorf("compile: in syntax template at %v: syntax subtemplate must contain a pattern variable determining expansion count", first.SourceLocation())
			}
			firstCompiled = Subtemplate{SyntaxTemplate{firstCompiled, patternVariables}, ellipsis, expansionPatternVariables}
		}
		patternVariables := map[*common.PatternVariable]int{}
		for patternVariable, n := range firstPatternVariables {
			patternVariables[patternVariable] = n
		}
		for patternVariable, rest := range restPatternVariables {
			if first, ok := patternVariables[patternVariable]; ok {
				if rest != first {
					return nil, nil, fmt.Errorf("compile: in syntax template at %v: incompatible expansion counts in first and rest of pair", syntax.SourceLocation())
				}
			}
			patternVariables[patternVariable] = rest
		}
		return common.Pair{firstCompiled, restCompiled}, patternVariables, nil
	}
	return syntax.Form(), map[*common.PatternVariable]int{}, nil
}

func compilePattern(syntax common.Syntax) (common.Datum, error) {
	if id, ok := syntax.Identifier(); ok {
		location := id.Location()
		if location == underscoreKeyword {
			return common.Underscore, nil
		}
		if location == ellipsisKeyword {
			return common.Ellipsis, nil
		}
		return syntax.Datum(), nil
	}
	if pair, ok := syntax.Pair(); ok {
		firstPattern := common.NewSyntax(pair.First)
		restPattern := common.NewSyntax(pair.Rest)
		first, err := compilePattern(firstPattern)
		if err != nil {
			return nil, err
		}
		rest, err := compilePattern(restPattern)
		if err != nil {
			return nil, err
		}
		return common.Pair{first, rest}, nil
	}
	return syntax.Datum(), nil
}
