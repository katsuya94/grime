package eval

import (
	"github.com/katsuya94/grime/common"
)

type beginFirstEvaluated struct {
	env   common.Environment
	forms []common.Datum
}

func (c beginFirstEvaluated) Call(d common.Datum) (common.EvaluationResult, error) {
	if len(c.forms) == 0 {
		return common.CallC(c.env, d)
	}
	return common.EvalC(
		c.env.SetContinuation(beginFirstEvaluated{c.env, c.forms[1:]}),
		c.forms[0],
	)
}
