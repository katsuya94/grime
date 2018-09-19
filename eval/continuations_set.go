package eval

import (
	"github.com/katsuya94/grime/common"
)

type setExpressionEvaluated struct {
	env      common.Environment
	variable *common.Variable
}

func (c setExpressionEvaluated) Call(d common.Datum) (common.EvaluationResult, error) {
	(*c.variable).Value = d
	return common.CallC(c.env, common.Void)
}
