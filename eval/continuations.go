package eval

import (
	"fmt"

	"github.com/katsuya94/grime/common"
)

type applicationProcedureEvaluated struct {
	env       common.Environment
	arguments []common.Expression
}

func (c applicationProcedureEvaluated) Call(d common.Datum) (common.EvaluationResult, error) {
	return partialEvaluationResult(c.env, d, nil, c.arguments)
}

type applicationArgumentEvaluated struct {
	env        common.Environment
	procedureV common.Datum
	argumentsV []common.Datum
	arguments  []common.Expression
}

func (c applicationArgumentEvaluated) Call(d common.Datum) (common.EvaluationResult, error) {
	return partialEvaluationResult(
		c.env,
		c.procedureV,
		append(c.argumentsV, d),
		c.arguments,
	)
}

func partialEvaluationResult(env common.Environment, procedureV common.Datum, argumentsV []common.Datum, arguments []common.Expression) (common.EvaluationResult, error) {
	if len(arguments) == 0 {
		return Apply(env, procedureV, argumentsV...)
	}
	return common.EvalC(
		env.SetContinuation(applicationArgumentEvaluated{
			env,
			procedureV,
			argumentsV,
			arguments[1:],
		}),
		arguments[0],
	)
}

func Apply(env common.Environment, procedureV common.Datum, argumentsV ...common.Datum) (common.EvaluationResult, error) {
	p, ok := procedureV.(common.Procedure)
	if !ok {
		return nil, fmt.Errorf("application: non-procedure in procedure position")
	}
	return p.Call(env, argumentsV...)
}

type beginFirstEvaluated struct {
	env         common.Environment
	expressions []common.Expression
}

func (c beginFirstEvaluated) Call(d common.Datum) (common.EvaluationResult, error) {
	if len(c.expressions) == 0 {
		return common.CallC(c.env, d)
	}
	return common.EvalC(
		c.env.SetContinuation(beginFirstEvaluated{c.env, c.expressions[1:]}),
		c.expressions[0],
	)
}

type ifConditionEvaluated struct {
	env       common.Environment
	then      common.Expression
	otherwise common.Expression
}

func (c ifConditionEvaluated) Call(d common.Datum) (common.EvaluationResult, error) {
	if d == common.Boolean(false) {
		return common.EvalC(c.env, c.otherwise)
	} else {
		return common.EvalC(c.env, c.then)
	}
}

type letInitEvaluated struct {
	env      common.Environment
	variable *common.Variable
	body     common.Expression
}

func (c letInitEvaluated) Call(d common.Datum) (common.EvaluationResult, error) {
	(*c.variable).Value = d
	return common.EvalC(
		c.env.SetContinuation(letBodyEvaluated{c.env}),
		c.body,
	)
}

type letBodyEvaluated struct {
	env common.Environment
}

func (c letBodyEvaluated) Call(d common.Datum) (common.EvaluationResult, error) {
	return common.CallC(c.env, d)
}

type setExpressionEvaluated struct {
	env      common.Environment
	variable *common.Variable
}

func (c setExpressionEvaluated) Call(d common.Datum) (common.EvaluationResult, error) {
	(*c.variable).Value = d
	return common.CallC(c.env, common.Void)
}
