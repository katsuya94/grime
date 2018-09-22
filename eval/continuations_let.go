package eval

import (
	"github.com/katsuya94/grime/common"
)

type letInitEvaluated struct {
	env  common.Environment
	name common.Symbol
	body common.Expression
}

func (c letInitEvaluated) Call(d common.Datum) (common.EvaluationResult, error) {
	return common.EvalC(
		c.env.Set(c.name, &common.Variable{d}).SetContinuation(letBodyEvaluated{c.env}),
		c.body,
	)
}

type letBodyEvaluated struct {
	env common.Environment
}

func (c letBodyEvaluated) Call(d common.Datum) (common.EvaluationResult, error) {
	return common.CallC(c.env, d)
}
