package eval

import (
	"github.com/katsuya94/grime/common"
)

type letInitEvaluated struct {
	env  common.Environment
	name common.Symbol
	body common.Datum
}

func (c letInitEvaluated) Call(d common.Datum) (common.EvaluationResult, error) {
	return common.FurtherEvaluation{
		c.env.Set(c.name, common.Variable{d}),
		c.body,
	}, nil
}
