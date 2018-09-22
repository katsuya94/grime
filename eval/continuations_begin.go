package eval

import (
	"github.com/katsuya94/grime/common"
)

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
