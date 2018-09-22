package eval

import (
	"github.com/katsuya94/grime/common"
)

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
