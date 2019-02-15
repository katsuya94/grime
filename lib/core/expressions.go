package core

import (
	"fmt"

	"github.com/katsuya94/grime/common"
)

// Application evaluates its procedure with its arguments.
type Application struct {
	Procedure common.Expression
	Arguments []common.Expression
}

func (e Application) Evaluate(c common.Continuation) (common.EvaluationResult, error) {
	return common.EvalC(
		applicationProcedureEvaluated{c, e.Arguments},
		e.Procedure,
	)
}

type applicationProcedureEvaluated struct {
	continuation common.Continuation
	arguments    []common.Expression
}

func (c applicationProcedureEvaluated) Call(d common.Datum) (common.EvaluationResult, error) {
	return partialEvaluationResult(c.continuation, d, nil, c.arguments)
}

type applicationArgumentEvaluated struct {
	continuation common.Continuation
	procedureV   common.Datum
	argumentsV   []common.Datum
	arguments    []common.Expression
}

func (c applicationArgumentEvaluated) Call(d common.Datum) (common.EvaluationResult, error) {
	return partialEvaluationResult(
		c.continuation,
		c.procedureV,
		append(c.argumentsV, d),
		c.arguments,
	)
}

func partialEvaluationResult(c common.Continuation, procedureV common.Datum, argumentsV []common.Datum, arguments []common.Expression) (common.EvaluationResult, error) {
	if len(arguments) == 0 {
		return common.Apply(c, procedureV, argumentsV...)
	}
	return common.EvalC(
		applicationArgumentEvaluated{
			c,
			procedureV,
			argumentsV,
			arguments[1:],
		},
		arguments[0],
	)
}

// If evaluates its condition, then evaluates its then or else branches accordingly.
type If struct {
	Condition common.Expression
	Then      common.Expression
	Else      common.Expression
}

func (e If) Evaluate(c common.Continuation) (common.EvaluationResult, error) {
	return common.EvalC(
		ifConditionEvaluated{c, e.Then, e.Else},
		e.Condition,
	)
}

type ifConditionEvaluated struct {
	continuation common.Continuation
	then         common.Expression
	otherwise    common.Expression
}

func (c ifConditionEvaluated) Call(d common.Datum) (common.EvaluationResult, error) {
	if d == common.Boolean(false) {
		return common.EvalC(c.continuation, c.otherwise)
	} else {
		return common.EvalC(c.continuation, c.then)
	}
}

// Let evaluates its init expression and assigns it to its variable before evaluating its body.
type Let struct {
	Variable *common.Variable
	Init     common.Expression
	Body     common.Expression
}

func (e Let) Evaluate(c common.Continuation) (common.EvaluationResult, error) {
	return common.EvalC(
		letInitEvaluated{c, e.Variable, e.Body},
		e.Init,
	)
}

type letInitEvaluated struct {
	continuation common.Continuation
	variable     *common.Variable
	body         common.Expression
}

func (c letInitEvaluated) Call(d common.Datum) (common.EvaluationResult, error) {
	(*c.variable).Value = d
	return common.EvalC(
		letBodyEvaluated{c.continuation},
		c.body,
	)
}

type letBodyEvaluated struct {
	continuation common.Continuation
}

func (c letBodyEvaluated) Call(d common.Datum) (common.EvaluationResult, error) {
	return common.CallC(c.continuation, d)
}

// Begin evaluates its expressions in order.
type Begin struct {
	Expressions []common.Expression
}

func (e Begin) Evaluate(c common.Continuation) (common.EvaluationResult, error) {
	return common.EvalC(
		beginFirstEvaluated{c, e.Expressions[1:]},
		e.Expressions[0],
	)
}

type beginFirstEvaluated struct {
	continuation common.Continuation
	expressions  []common.Expression
}

func (c beginFirstEvaluated) Call(d common.Datum) (common.EvaluationResult, error) {
	if len(c.expressions) == 0 {
		return common.CallC(c.continuation, d)
	}
	return common.EvalC(
		beginFirstEvaluated{c.continuation, c.expressions[1:]},
		c.expressions[0],
	)
}

// Define sets its variable to the value of its expression.
type Define struct {
	Variable   *common.Variable
	Expression common.Expression
}

func (e Define) Evaluate(c common.Continuation) (common.EvaluationResult, error) {
	return common.EvalC(
		defineExpressionEvaluated{c, e.Variable},
		e.Expression,
	)
}

type defineExpressionEvaluated struct {
	continuation common.Continuation
	variable     *common.Variable
}

func (c defineExpressionEvaluated) Call(d common.Datum) (common.EvaluationResult, error) {
	(*c.variable).Value = d
	return common.CallC(c.continuation, common.Void)
}

// Set sets its variable to the value of its expression.
type Set struct {
	Identifier common.Identifier
	Variable   *common.Variable
	Expression common.Expression
}

func (e Set) Evaluate(c common.Continuation) (common.EvaluationResult, error) {
	return common.EvalC(
		setExpressionEvaluated{c, e.Identifier, e.Variable},
		e.Expression,
	)
}

type setExpressionEvaluated struct {
	continuation common.Continuation
	identifier   common.Identifier
	variable     *common.Variable
}

func (c setExpressionEvaluated) Call(d common.Datum) (common.EvaluationResult, error) {
	if c.variable.Value == nil {
		return common.ErrorC(fmt.Errorf("evaluate: %v at %v: cannot set identifier before its definition", c.identifier.Name(), c.identifier.SourceLocation()))
	}
	(*c.variable).Value = d
	return common.CallC(c.continuation, common.Void)
}

// Reference evaluates to the value of its variable.
type Reference struct {
	Identifier common.Identifier
	Variable   *common.Variable
}

func (e Reference) Evaluate(c common.Continuation) (common.EvaluationResult, error) {
	if e.Variable.Value == nil {
		return common.ErrorC(fmt.Errorf("evaluate: %v at %v: cannot reference identifier before its definition", e.Identifier.Name(), e.Identifier.SourceLocation()))
	}
	return common.CallC(c, e.Variable.Value)
}

// SyntaxCase evaluates its input and evaluates to an output according to pattern matching and fender expressions.
type SyntaxCase struct {
	Input                   common.Expression
	Literals                map[common.Symbol]common.Location
	Patterns                []common.Datum
	PatternVariableBindings []map[common.Symbol]*common.PatternVariable
	Fenders                 []common.Expression
	Outputs                 []common.Expression
}

func (e SyntaxCase) Evaluate(c common.Continuation) (common.EvaluationResult, error) {
	return common.EvalC(
		syntaxCaseInputEvaluated{c, e.Literals, e.Patterns, e.PatternVariableBindings, e.Fenders, e.Outputs},
		e.Input,
	)
}

type syntaxCaseInputEvaluated struct {
	continuation            common.Continuation
	literals                map[common.Symbol]common.Location
	patterns                []common.Datum
	patternVariableBindings []map[common.Symbol]*common.PatternVariable
	fenders                 []common.Expression
	outputs                 []common.Expression
}

func (c syntaxCaseInputEvaluated) Call(d common.Datum) (common.EvaluationResult, error) {
	if !common.IsSyntax(d) {
		return nil, fmt.Errorf("syntax-case: expected syntax")
	}
	return syntaxCaseMatch(c.continuation, d, c.literals, c.patterns, c.patternVariableBindings, c.fenders, c.outputs)
}

type syntaxCaseFenderEvaluated struct {
	continuation            common.Continuation
	input                   common.Datum
	literals                map[common.Symbol]common.Location
	output                  common.Expression
	patterns                []common.Datum
	patternVariableBindings []map[common.Symbol]*common.PatternVariable
	fenders                 []common.Expression
	outputs                 []common.Expression
}

func (c syntaxCaseFenderEvaluated) Call(d common.Datum) (common.EvaluationResult, error) {
	if d == common.Boolean(false) {
		return syntaxCaseMatch(c.continuation, c.input, c.literals, c.patterns, c.patternVariableBindings, c.fenders, c.outputs)
	}
	return common.EvalC(c.continuation, c.output)
}

func syntaxCaseMatch(continuation common.Continuation, input common.Datum, literals map[common.Symbol]common.Location, patterns []common.Datum, patternVariableBindings []map[common.Symbol]*common.PatternVariable, fenders []common.Expression, outputs []common.Expression) (common.EvaluationResult, error) {
	syntax := common.NewSyntax(input)
	for i := range patterns {
		result, ok, err := common.MatchSyntax(syntax, patterns[i], literals)
		if err != nil {
			return nil, err
		} else if ok {
			for name, match := range result {
				(*patternVariableBindings[i][name]).Match = match
			}
			return common.EvalC(
				syntaxCaseFenderEvaluated{
					continuation,
					input,
					literals,
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
	return nil, fmt.Errorf("in macro use at %v: bad syntax", syntax.SourceLocation())
}

// SyntaxTemplate evaluates to syntax handling repitition and pattern variable references accordingly.
type SyntaxTemplate struct {
	Template         common.Datum
	PatternVariables []*common.PatternVariable
}

func (e SyntaxTemplate) Evaluate(c common.Continuation) (common.EvaluationResult, error) {
	bindings := make(map[*common.PatternVariable]interface{}, len(e.PatternVariables))
	for _, patternVariable := range e.PatternVariables {
		bindings[patternVariable] = patternVariable.Match
	}
	datum, err := evaluateSyntaxTemplate(e.Template, bindings)
	if err != nil {
		return common.ErrorC(err)
	}
	return common.CallC(c, datum)
}

func evaluateSyntaxTemplate(datum common.Datum, bindings map[*common.PatternVariable]interface{}) (common.Datum, error) {
	switch datum := datum.(type) {
	case common.WrappedSyntax:
		return datum, nil
	case PatternVariableReference:
		return bindings[datum.PatternVariable].(common.Syntax).Form(), nil
	case common.Pair:
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
				result = common.Pair{data[i], result}
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
		return common.Pair{first, rest}, nil
	default:
		return nil, fmt.Errorf("evaluate: unhandled syntax template %#v", datum)
	}
}

func evaluateSubtemplate(subtemplate Subtemplate, bindings map[*common.PatternVariable]interface{}) ([]common.Datum, error) {
	if subtemplate.Nesting == 0 {
		datum, err := evaluateSyntaxTemplate(subtemplate.Subtemplate.Template, bindings)
		if err != nil {
			return nil, err
		}
		return []common.Datum{datum}, nil
	}
	n := len(bindings[subtemplate.PatternVariables[0]].([]interface{}))
	for _, patternVariable := range subtemplate.PatternVariables {
		if len(bindings[patternVariable].([]interface{})) != n {
			return nil, fmt.Errorf("evaluate: differing number of matches for syntax template")
		}
	}
	var data []common.Datum
	for j := 0; j < n; j++ {
		nestedBindings := make(map[*common.PatternVariable]interface{}, len(subtemplate.Subtemplate.PatternVariables))
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

// Subtemplate represents the repitition of a SyntaxTemplate.
type Subtemplate struct {
	Subtemplate      SyntaxTemplate
	Nesting          int
	PatternVariables []*common.PatternVariable
}

// PatternVariableReference represents a reference to a pattern variable.
type PatternVariableReference struct {
	PatternVariable *common.PatternVariable
}

// Literal represents a literal value.

type Literal struct {
	Datum common.Datum
}

func (e Literal) Evaluate(c common.Continuation) (common.EvaluationResult, error) {
	return common.CallC(c, e.Datum)
}
