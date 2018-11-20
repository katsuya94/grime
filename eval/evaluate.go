package eval

import (
	"fmt"

	"github.com/katsuya94/grime/common"
)

// TODO: this is a thin wrapper, remove.
func EvaluateExpression(c common.Continuation, expression common.Expression) (common.EvaluationResult, error) {
	if expression == nil {
		return common.CallC(c, nil)
	}
	return expression.Evaluate(c)
}

type escapeEvaluated struct{}

func (escapeEvaluated) Call(d common.Datum) (common.EvaluationResult, error) {
	return nil, fmt.Errorf("evaluate: escape continuation called without escaping context")
}

// perform further evaluation or call continuations as necessary until the escape continuation is called.
func CallWithEscape(evaluationResultFactory func(common.Continuation) (common.EvaluationResult, error)) (common.Datum, error) {
	escape := escapeEvaluated{}
	evaluationResult, err := evaluationResultFactory(&escape)
	if err != nil {
		return nil, err
	}
	for {
		switch v := evaluationResult.(type) {
		case common.FurtherEvaluation:
			evaluationResult, err = EvaluateExpression(v.Continuation, v.Expression)
			if err != nil {
				return nil, err
			}
		case common.ContinuationCall:
			if v.Continuation == &escape {
				return v.Value, nil
			}
			evaluationResult, err = v.Continuation.Call(v.Value)
			if err != nil {
				return nil, err
			}
		default:
			return nil, fmt.Errorf("evaluate: unhandled evaluation result: %#v", v)
		}
	}
}

func EvaluateExpressionOnce(expression common.Expression) (common.Datum, error) {
	return CallWithEscape(func(escape common.Continuation) (common.EvaluationResult, error) {
		return EvaluateExpression(escape, expression)
	})
}
