package core

import (
	"fmt"

	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/util"
)

type Compiler struct {
	BodyCompiler       BodyCompiler
	ExpressionCompiler ExpressionCompiler
	Expander           Expander
	Phase              int
}

func NewCompiler() Compiler {
	return Compiler{
		BodyCompiler:       BodyCompile,
		ExpressionCompiler: ExpressionCompile,
		Expander:           Expand,
		Phase:              0,
	}
}

func (compiler Compiler) BodyCompile(forms []common.Datum, defined []common.WrappedSyntax) (common.Expression, []common.WrappedSyntax, error) {
	return compiler.BodyCompiler(compiler, forms, defined)
}

func (compiler Compiler) ExpressionCompile(form common.Datum) (common.Expression, error) {
	return compiler.ExpressionCompiler(compiler, form)
}

func (compiler Compiler) Expand(form common.Datum) (common.Datum, bool, error) {
	return compiler.Expander(compiler, form)
}

func (compiler Compiler) ExpandCompletely(form common.Datum) (common.Datum, error) {
	for {
		expanded, ok, err := compiler.Expand(form)
		if err != nil {
			return nil, err
		} else if !ok {
			return form, nil
		}
		form = expanded
	}
}

func (compiler Compiler) Next() Compiler {
	compiler.Phase += 1
	return compiler
}

func Compile(body common.WrappedSyntax) (common.Expression, []common.WrappedSyntax, error) {
	defined := body.DefinedAt(0)
	var forms []common.Datum
	err := util.Each(body.Datum(), func(datum common.Datum) error {
		forms = append(forms, body.PushOnto(datum))
		return nil
	})
	if err != nil {
		return nil, nil, err
	}
	return NewCompiler().BodyCompile(forms, defined)
}

type BodyCompiler func(compiler Compiler, forms []common.Datum, defined []common.WrappedSyntax) (common.Expression, []common.WrappedSyntax, error)

func BodyCompile(compiler Compiler, forms []common.Datum, defined []common.WrappedSyntax) (common.Expression, []common.WrappedSyntax, error) {
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
				form := v.Form
				keyword := &common.Keyword{}
				err := define(v.Identifier, compiler.Phase, keyword, &form, forms[i+1:], &defined)
				if err != nil {
					return nil, nil, err
				}
				expression, err := compiler.Next().ExpressionCompile(form)
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
				processed = true
			case DefineForm:
				form := v.Form
				variable := &common.Variable{}
				err := define(v.Identifier, compiler.Phase, variable, &form, forms[i+1:], &defined)
				if err != nil {
					return nil, nil, err
				}
				definitionVariables = append(definitionVariables, variable)
				definitionForms = append(definitionForms, form)
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
					v, ok, err := compiler.Expand(syntax)
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
		expression, err := compiler.ExpressionCompile(definitionForms[i])
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
		expression, err := compiler.ExpressionCompile(form)
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
	return expression, defined, nil
}

func define(identifier common.WrappedSyntax, phase int, location common.Location, form *common.Datum, rest []common.Datum, defined *[]common.WrappedSyntax) error {
	name, l := identifier.IdentifierAt(phase)
	if l != nil {
		for _, id := range *defined {
			if identifier.IdentifierEquals(id) {
				return fmt.Errorf("compile: %v: already defined", name)
			}
		}
	}
	if identifier.Unmarked() {
		*defined = append(*defined, identifier)
	}
	*form = syntaxSet(*form, name, location)
	for i := 0; i < len(rest); i++ {
		rest[i] = syntaxSet(rest[i], name, location)
	}
	return nil
}

type ExpressionCompiler func(compiler Compiler, form common.Datum) (common.Expression, error)

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
		expression, _, err := compiler.BodyCompile(form.Forms, nil)
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
		name, _ := form.Identifier.IdentifierAt(compiler.Phase)
		body := syntaxSet(form.Body, name, variable)
		bodyExpression, err := compiler.ExpressionCompile(body)
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
		body := form.Body
		for _, formal := range form.Formals {
			name, _ := formal.IdentifierAt(compiler.Phase)
			variable := &common.Variable{}
			body = syntaxSet(body, name, variable)
			variables = append(variables, variable)
		}
		expression, err := compiler.ExpressionCompile(body)
		if err != nil {
			return nil, err
		}
		return Literal{common.Lambda{variables, expression}}, nil
	case ReferenceForm:
		name, location := form.Identifier.IdentifierAt(compiler.Phase)
		if location == nil {
			return nil, fmt.Errorf("compile: unbound identifier %v", name)
		}
		variable, ok := location.(*common.Variable)
		if !ok {
			return nil, fmt.Errorf("compile: non-variable identifier %v in expression context", name)
		}
		return Reference{variable}, nil
	case SetForm:
		name, location := form.Identifier.IdentifierAt(compiler.Phase)
		if location == nil {
			return nil, fmt.Errorf("compile: unbound identifier %v", name)
		}
		variable, ok := location.(*common.Variable)
		if !ok {
			return nil, fmt.Errorf("compile: non-variable identifier %v in assignment", name)
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
			name, location := literal.IdentifierAt(compiler.Phase)
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
			pattern, err := compilePattern(form.Patterns[i], compiler.Phase)
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
				return nil, fmt.Errorf("compile: unhandled literal %#v", datum)
			}
		}
	default:
		return nil, fmt.Errorf("compile: unexpected form in expression context %#v", form)
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
			return nil, nil, fmt.Errorf("compile: encountered unwrapped symbol in syntax template")
		}
		_, location := syntax.IdentifierAt(phase)
		if patternVariable, ok := location.(*common.PatternVariable); ok {
			return PatternVariableReference{patternVariable}, map[*common.PatternVariable]int{patternVariable: patternVariable.Nesting}, nil
		}
		if location == ellipsisKeyword {
			return nil, nil, fmt.Errorf("compile: improper use of ellipsis in syntax template")
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
			first := pair.First
			if isSyntax {
				first = syntax.PushOnto(first)
			}
			identifier, ok := first.(common.WrappedSyntax)
			if !ok {
				break
			}
			if !identifier.IsIdentifier() {
				break
			}
			_, location := identifier.IdentifierAt(phase)
			if location != ellipsisKeyword {
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
		return nil, nil, fmt.Errorf("compile: unhandled syntax template form %#v", datum)
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
			return nil, fmt.Errorf("compile: encountered unwrapped symbol in pattern")
		}
		_, location := syntax.IdentifierAt(phase)
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
			firstPattern = syntax.PushOnto(datum.First)
			restPattern = syntax.PushOnto(datum.Rest)
		} else {
			firstPattern = datum.First
			restPattern = datum.Rest
		}
		first, err := compilePattern(firstPattern, 1)
		if err != nil {
			return nil, err
		}
		rest, err := compilePattern(restPattern, 1)
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
