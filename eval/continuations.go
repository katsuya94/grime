package eval

import (
	"fmt"

	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/util"
)

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
		return Apply(c, procedureV, argumentsV...)
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

func Apply(c common.Continuation, procedureV common.Datum, argumentsV ...common.Datum) (common.EvaluationResult, error) {
	p, ok := procedureV.(common.Procedure)
	if !ok {
		return nil, fmt.Errorf("application: non-procedure in procedure position")
	}
	return p.Call(c, argumentsV...)
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

type defineExpressionEvaluated struct {
	continuation common.Continuation
	variable     *common.Variable
}

func (c defineExpressionEvaluated) Call(d common.Datum) (common.EvaluationResult, error) {
	(*c.variable).Defined = true
	(*c.variable).Value = d
	return common.CallC(c.continuation, common.Void)
}

type setExpressionEvaluated struct {
	continuation common.Continuation
	variable     *common.Variable
}

func (c setExpressionEvaluated) Call(d common.Datum) (common.EvaluationResult, error) {
	if !c.variable.Defined {
		return nil, fmt.Errorf("evaluate: cannot set identifier before its definition")
	}
	(*c.variable).Value = d
	return common.CallC(c.continuation, common.Void)
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
	syntax, ok := d.(common.WrappedSyntax)
	if !ok {
		return nil, fmt.Errorf("syntax-case: expected syntax")
	}
	return syntaxCaseMatch(c.continuation, syntax, c.literals, c.patterns, c.patternVariableBindings, c.fenders, c.outputs)
}

type syntaxCaseFenderEvaluated struct {
	continuation            common.Continuation
	input                   common.WrappedSyntax
	literals                map[common.Symbol]common.Location
	result                  map[common.Symbol]interface{}
	bindings                map[common.Symbol]*common.PatternVariable
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
	for name, match := range c.result {
		(*c.bindings[name]).Match = match
	}
	return common.EvalC(c.continuation, c.output)
}

func syntaxCaseMatch(continuation common.Continuation, input common.WrappedSyntax, literals map[common.Symbol]common.Location, patterns []common.Datum, patternVariableBindings []map[common.Symbol]*common.PatternVariable, fenders []common.Expression, outputs []common.Expression) (common.EvaluationResult, error) {
	for i := range patterns {
		result, ok, err := util.MatchSyntax(input, patterns[i], literals)
		if err != nil {
			return nil, err
		} else if ok {
			return common.EvalC(
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
	return nil, fmt.Errorf("syntax-case: bad syntax")
}
