package eval

import (
	"github.com/katsuya94/grime/common"
)

type ifConditionEvaluated struct {
	env   common.Environment
	then  common.Datum
	_else common.Datum
}

func (c ifConditionEvaluated) Call(d common.Datum) (common.EvaluationResult, error) {
	if d == common.Boolean(false) {
		return common.FurtherEvaluation{c.env, c._else}, nil
	} else {
		return common.FurtherEvaluation{c.env, c.then}, nil
	}
}
