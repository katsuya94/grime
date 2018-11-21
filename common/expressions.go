package common

import "fmt"

// Application evaluates its procedure with its arguments.
type Application struct {
	Procedure Expression
	Arguments []Expression
}

func (e Application) Evaluate(c Continuation) (EvaluationResult, error) {
	return EvalC(
		applicationProcedureEvaluated{c, e.Arguments},
		e.Procedure,
	)
}

type applicationProcedureEvaluated struct {
	continuation Continuation
	arguments    []Expression
}

func (c applicationProcedureEvaluated) Call(d Datum) (EvaluationResult, error) {
	return partialEvaluationResult(c.continuation, d, nil, c.arguments)
}

type applicationArgumentEvaluated struct {
	continuation Continuation
	procedureV   Datum
	argumentsV   []Datum
	arguments    []Expression
}

func (c applicationArgumentEvaluated) Call(d Datum) (EvaluationResult, error) {
	return partialEvaluationResult(
		c.continuation,
		c.procedureV,
		append(c.argumentsV, d),
		c.arguments,
	)
}

func partialEvaluationResult(c Continuation, procedureV Datum, argumentsV []Datum, arguments []Expression) (EvaluationResult, error) {
	if len(arguments) == 0 {
		return Apply(c, procedureV, argumentsV...)
	}
	return EvalC(
		applicationArgumentEvaluated{
			c,
			procedureV,
			argumentsV,
			arguments[1:],
		},
		arguments[0],
	)
}

func Apply(c Continuation, procedureV Datum, argumentsV ...Datum) (EvaluationResult, error) {
	p, ok := procedureV.(Procedure)
	if !ok {
		return nil, fmt.Errorf("application: non-procedure in procedure position")
	}
	return p.Call(c, argumentsV...)
}

// If evaluates its condition, then evaluates its then or else branches accordingly.
type If struct {
	Condition Expression
	Then      Expression
	Else      Expression
}

func (e If) Evaluate(c Continuation) (EvaluationResult, error) {
	return EvalC(
		ifConditionEvaluated{c, e.Then, e.Else},
		e.Condition,
	)
}

type ifConditionEvaluated struct {
	continuation Continuation
	then         Expression
	otherwise    Expression
}

func (c ifConditionEvaluated) Call(d Datum) (EvaluationResult, error) {
	if d == Boolean(false) {
		return EvalC(c.continuation, c.otherwise)
	} else {
		return EvalC(c.continuation, c.then)
	}
}

// Let evaluates its init expression and assigns it to its variable before evaluating its body.
type Let struct {
	Variable *Variable
	Init     Expression
	Body     Expression
}

func (e Let) Evaluate(c Continuation) (EvaluationResult, error) {
	return EvalC(
		letInitEvaluated{c, e.Variable, e.Body},
		e.Init,
	)
}

type letInitEvaluated struct {
	continuation Continuation
	variable     *Variable
	body         Expression
}

func (c letInitEvaluated) Call(d Datum) (EvaluationResult, error) {
	(*c.variable).Value = d
	return EvalC(
		letBodyEvaluated{c.continuation},
		c.body,
	)
}

type letBodyEvaluated struct {
	continuation Continuation
}

func (c letBodyEvaluated) Call(d Datum) (EvaluationResult, error) {
	return CallC(c.continuation, d)
}

// Begin evaluates its expressions in order.
type Begin struct {
	Expressions []Expression
}

func (e Begin) Evaluate(c Continuation) (EvaluationResult, error) {
	return EvalC(
		beginFirstEvaluated{c, e.Expressions[1:]},
		e.Expressions[0],
	)
}

type beginFirstEvaluated struct {
	continuation Continuation
	expressions  []Expression
}

func (c beginFirstEvaluated) Call(d Datum) (EvaluationResult, error) {
	if len(c.expressions) == 0 {
		return CallC(c.continuation, d)
	}
	return EvalC(
		beginFirstEvaluated{c.continuation, c.expressions[1:]},
		c.expressions[0],
	)
}

// Define sets its variable to the value of its expression.
type Define struct {
	Variable   *Variable
	Expression Expression
}

func (e Define) Evaluate(c Continuation) (EvaluationResult, error) {
	return EvalC(
		defineExpressionEvaluated{c, e.Variable},
		e.Expression,
	)
}

type defineExpressionEvaluated struct {
	continuation Continuation
	variable     *Variable
}

func (c defineExpressionEvaluated) Call(d Datum) (EvaluationResult, error) {
	(*c.variable).Defined = true
	(*c.variable).Value = d
	return CallC(c.continuation, Void)
}

// Set sets its variable to the value of its expression.
type Set struct {
	Variable   *Variable
	Expression Expression
}

func (e Set) Evaluate(c Continuation) (EvaluationResult, error) {
	return EvalC(
		setExpressionEvaluated{c, e.Variable},
		e.Expression,
	)
}

type setExpressionEvaluated struct {
	continuation Continuation
	variable     *Variable
}

func (c setExpressionEvaluated) Call(d Datum) (EvaluationResult, error) {
	if !c.variable.Defined {
		return nil, fmt.Errorf("evaluate: cannot set identifier before its definition")
	}
	(*c.variable).Value = d
	return CallC(c.continuation, Void)
}

// Reference evaluates to the value of its variable.
type Reference struct {
	Variable *Variable
}

func (e Reference) Evaluate(c Continuation) (EvaluationResult, error) {
	if !e.Variable.Defined {
		return ErrorC(fmt.Errorf("evaluate: cannot reference identifier before its definition"))
	}
	return CallC(c, e.Variable.Value)
}

// SyntaxCase evaluates its input and evaluates to an output according to pattern matching and fender expressions.
type SyntaxCase struct {
	Input                   Expression
	Literals                map[Symbol]Location
	Patterns                []Datum
	PatternVariableBindings []map[Symbol]*PatternVariable
	Fenders                 []Expression
	Outputs                 []Expression
}

func (e SyntaxCase) Evaluate(c Continuation) (EvaluationResult, error) {
	return EvalC(
		syntaxCaseInputEvaluated{c, e.Literals, e.Patterns, e.PatternVariableBindings, e.Fenders, e.Outputs},
		e.Input,
	)
}

type syntaxCaseInputEvaluated struct {
	continuation            Continuation
	literals                map[Symbol]Location
	patterns                []Datum
	patternVariableBindings []map[Symbol]*PatternVariable
	fenders                 []Expression
	outputs                 []Expression
}

func (c syntaxCaseInputEvaluated) Call(d Datum) (EvaluationResult, error) {
	if !IsSyntax(d) {
		return nil, fmt.Errorf("syntax-case: expected syntax")
	}
	return syntaxCaseMatch(c.continuation, d, c.literals, c.patterns, c.patternVariableBindings, c.fenders, c.outputs)
}

type syntaxCaseFenderEvaluated struct {
	continuation            Continuation
	input                   Datum
	literals                map[Symbol]Location
	result                  map[Symbol]interface{}
	bindings                map[Symbol]*PatternVariable
	output                  Expression
	patterns                []Datum
	patternVariableBindings []map[Symbol]*PatternVariable
	fenders                 []Expression
	outputs                 []Expression
}

func (c syntaxCaseFenderEvaluated) Call(d Datum) (EvaluationResult, error) {
	if d == Boolean(false) {
		return syntaxCaseMatch(c.continuation, c.input, c.literals, c.patterns, c.patternVariableBindings, c.fenders, c.outputs)
	}
	for name, match := range c.result {
		(*c.bindings[name]).Match = match
	}
	return EvalC(c.continuation, c.output)
}

func syntaxCaseMatch(continuation Continuation, input Datum, literals map[Symbol]Location, patterns []Datum, patternVariableBindings []map[Symbol]*PatternVariable, fenders []Expression, outputs []Expression) (EvaluationResult, error) {
	for i := range patterns {
		fmt.Printf("%#v\n%#v\n\n", input, patterns[i])
		result, ok, err := MatchSyntax(input, patterns[i], literals)
		if err != nil {
			return nil, err
		} else if ok {
			return EvalC(
				syntaxCaseFenderEvaluated{
					continuation,
					input,
					literals,
					result,
					patternVariableBindings[i],
					outputs[i],
					patterns[i+1:],
					patternVariableBindings[i+1:],
					fenders[i+1:],
					outputs[i+1:],
				},
				fenders[i],
			)
		}
	}
	return nil, fmt.Errorf("bad syntax")
}

// SyntaxTemplate evaluates to syntax handling repitition and pattern variable references accordingly.
type SyntaxTemplate struct {
	Template         Datum
	PatternVariables []*PatternVariable
}

func (e SyntaxTemplate) Evaluate(c Continuation) (EvaluationResult, error) {
	bindings := make(map[*PatternVariable]interface{}, len(e.PatternVariables))
	for _, patternVariable := range e.PatternVariables {
		bindings[patternVariable] = patternVariable.Match
	}
	datum, err := evaluateSyntaxTemplate(e.Template, bindings)
	if err != nil {
		return ErrorC(err)
	}
	return CallC(c, datum)
}

func evaluateSyntaxTemplate(datum Datum, bindings map[*PatternVariable]interface{}) (Datum, error) {
	switch datum := datum.(type) {
	case WrappedSyntax:
		return datum, nil
	case PatternVariableReference:
		return bindings[datum.PatternVariable].(Datum), nil
	case Pair:
		if first, ok := datum.First.(Subtemplate); ok {
			data, err := evaluateSubtemplate(first, bindings)
			if err != nil {
				return nil, err
			}
			rest, err := evaluateSyntaxTemplate(datum.Rest, bindings)
			if err != nil {
				return nil, err
			}
			result := rest
			for i := len(data) - 1; i >= 0; i-- {
				result = Pair{data[i], result}
			}
			return result, nil
		}
		first, err := evaluateSyntaxTemplate(datum.First, bindings)
		if err != nil {
			return nil, err
		}
		rest, err := evaluateSyntaxTemplate(datum.Rest, bindings)
		if err != nil {
			return nil, err
		}
		return Pair{first, rest}, nil
	default:
		return nil, fmt.Errorf("evaluate: unhandled syntax template %#v", datum)
	}
}

func evaluateSubtemplate(subtemplate Subtemplate, bindings map[*PatternVariable]interface{}) ([]Datum, error) {
	if subtemplate.Nesting == 0 {
		datum, err := evaluateSyntaxTemplate(subtemplate.Subtemplate.Template, bindings)
		if err != nil {
			return nil, err
		}
		return []Datum{datum}, nil
	}
	n := len(bindings[subtemplate.PatternVariables[0]].([]interface{}))
	for _, patternVariable := range subtemplate.PatternVariables {
		if len(bindings[patternVariable].([]interface{})) != n {
			return nil, fmt.Errorf("evaluate: differing number of matches for syntax template")
		}
	}
	var data []Datum
	for j := 0; j < n; j++ {
		nestedBindings := make(map[*PatternVariable]interface{}, len(subtemplate.Subtemplate.PatternVariables))
		for _, patternVariable := range subtemplate.Subtemplate.PatternVariables {
			nestedBindings[patternVariable] = bindings[patternVariable]
		}
		for _, patternVariable := range subtemplate.PatternVariables {
			nestedBindings[patternVariable] = nestedBindings[patternVariable].([]interface{})[j]
		}
		nestedData, err := evaluateSubtemplate(Subtemplate{subtemplate.Subtemplate, subtemplate.Nesting - 1, subtemplate.PatternVariables}, nestedBindings)
		if err != nil {
			return nil, err
		}
		data = append(data, nestedData...)
	}
	return data, nil
}
