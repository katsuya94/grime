package eval

import (
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
		return nil, Errorf("application: non-procedure in procedure position")
	}
	return p.Call(env, argumentsV...)
}
