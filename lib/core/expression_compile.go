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
		template, patternVariablesUnexpanded, err := compileTemplate(form.Datum, compiler.Phase)
		if err != nil {
			return nil, err
		}
		var patternVariables []*common.PatternVariable
		for patternVariable, n := range patternVariablesUnexpanded {
			if n > 0 {
				return nil, fmt.Errorf("compile: encountered unexpanded pattern variable")
			}
			patternVariables = append(patternVariables, patternVariable)
		}
		return SyntaxTemplate{template, patternVariables}, nil
	case BeginForm:
		scope := common.NewScope(0)
		forms := make([]common.Datum, len(form.Forms))
		for i := range form.Forms {
			forms[i] = common.Syntax{form.Forms[i]}.Push(scope).Datum
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
		scope := common.NewScope(0)
		err = scope.Set(form.Identifier, variable)
		if err != nil {
			return nil, err
		}
		forms := make([]common.Datum, len(form.Body))
		for i := range form.Body {
			forms[i] = common.Syntax{form.Body[i]}.Push(scope).Datum
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
		scope := common.NewScope(0)
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
			forms[i] = common.Syntax{form.Body[i]}.Push(scope).Datum
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
		return Reference{variable}, nil
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
		return Set{variable, expression}, nil
	case SyntaxCaseForm:
		inputExpression, err := compiler.ExpressionCompile(form.Input)
		if err != nil {
			return nil, err
		}
		literals := make(map[common.Symbol]common.Location)
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
			pattern, err := compilePattern(form.Patterns[i], compiler.Phase)
			if err != nil {
				return nil, err
			}
			patternVariables, err := common.PatternVariables(pattern, literals)
			if err != nil {
				return nil, err
			}
			bindings := make(map[common.Symbol]*common.PatternVariable)
			scope := common.NewScope(0)
			for name, n := range patternVariables {
				patternVariable := &common.PatternVariable{nil, n}
				bindings[name] = patternVariable
				err := scope.Set(common.NewIdentifier(name), patternVariable)
				if err != nil {
					return nil, err
				}
			}
			fender := common.Syntax{form.Fenders[i]}.Push(scope).Datum
			output := common.Syntax{form.Outputs[i]}.Push(scope).Datum
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
		return nil, fmt.Errorf("compile: unexpected form in expression context: %v", common.Write(form))
	}
}

func compileTemplate(datum common.Datum, phase int) (common.Datum, map[*common.PatternVariable]int, error) {
	syntax, isSyntax := datum.(common.WrappedSyntax)
	if isSyntax {
		datum = syntax.Datum()
	}
	switch datum := datum.(type) {
	case common.Boolean, common.Number, common.Character, common.String:
		return syntax, map[*common.PatternVariable]int{}, nil
	case common.Symbol:
		if !isSyntax {
			panic("encountered unwrapped symbol in syntax template")
		}
		id, _ := syntax.Identifier()
		location := id.Location()
		if patternVariable, ok := location.(*common.PatternVariable); ok {
			return PatternVariableReference{patternVariable}, map[*common.PatternVariable]int{patternVariable: patternVariable.Nesting}, nil
		}
		if location == ellipsisKeyword {
			return nil, nil, fmt.Errorf("compile: improper use of ellipsis in syntax template")
		}
		return syntax, map[*common.PatternVariable]int{}, nil
	case common.Pair:
		ellipsis := 0
		var (
			first common.Datum
			rest  common.Datum
		)
		if isSyntax {
			pair := syntax.PushDown().(common.Pair)
			first = pair.First
			rest = pair.Rest
		} else {
			first = datum.First
			rest = datum.Rest
		}
		for {
			var pair common.Pair
			if syntax, ok := rest.(common.WrappedSyntax); ok {
				_, ok := syntax.Datum().(common.Pair)
				if !ok {
					break
				}
				pair = syntax.PushDown().(common.Pair)
			} else {
				_, ok := rest.(common.Pair)
				if !ok {
					break
				}
				pair = rest.(common.Pair)
			}
			id, ok := common.Syntax{pair.First}.Identifier()
			if !ok {
				break
			}
			if id.Location() != ellipsisKeyword {
				break
			}
			ellipsis++
			rest = pair.Rest
		}
		firstTemplate := first
		restTemplate := rest
		firstCompiled, firstPatternVariables, err := compileTemplate(firstTemplate, phase)
		if err != nil {
			return nil, nil, err
		}
		_, firstStatic := firstCompiled.(common.WrappedSyntax)
		if firstStatic && ellipsis > 0 {
			return nil, nil, fmt.Errorf("compile: syntax subtemplate must contain a pattern variable")
		}
		restCompiled, restPatternVariables, err := compileTemplate(restTemplate, phase)
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
		for patternVariable, rest := range restPatternVariables {
			if first, ok := patternVariables[patternVariable]; ok {
				if rest != first {
					return nil, nil, fmt.Errorf("compile: incompatible expansion counts for pattern variable")
				}
			}
			patternVariables[patternVariable] = rest
		}
		return common.Pair{firstCompiled, restCompiled}, patternVariables, nil
	default:
		if datum == common.Null {
			return syntax, map[*common.PatternVariable]int{}, nil
		}
		return nil, nil, fmt.Errorf("compile: unexpected syntax template form: %v", common.Write(datum))
	}
}

func compilePattern(datum common.Datum, phase int) (common.Datum, error) {
	syntax, isSyntax := datum.(common.WrappedSyntax)
	if isSyntax {
		datum = syntax.Datum()
	}
	switch datum := datum.(type) {
	case common.Boolean, common.Number, common.Character, common.String:
		return datum, nil
	case common.Symbol:
		if !isSyntax {
			panic("encountered unwrapped symbol in pattern")
		}
		id, _ := syntax.Identifier()
		location := id.Location()
		if location == underscoreKeyword {
			return common.Underscore, nil
		}
		if location == ellipsisKeyword {
			return common.Ellipsis, nil
		}
		return datum, nil
	case common.Pair:
		var (
			firstPattern common.Datum
			restPattern  common.Datum
		)
		if isSyntax {
			pair := syntax.PushDown().(common.Pair)
			firstPattern = pair.First
			restPattern = pair.Rest
		} else {
			firstPattern = datum.First
			restPattern = datum.Rest
		}
		first, err := compilePattern(firstPattern, phase)
		if err != nil {
			return nil, err
		}
		rest, err := compilePattern(restPattern, phase)
		if err != nil {
			return nil, err
		}
		return common.Pair{first, rest}, nil
	default:
		if datum == common.Null {
			return common.Null, nil
		}
		return nil, fmt.Errorf("compile: unhandled pattern form: %v", common.Write(datum))
	}
}
