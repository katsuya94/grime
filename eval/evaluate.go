package eval

import (
	"fmt"

	"github.com/katsuya94/grime/common"
)

func EvaluateExpression(c common.Continuation, expression common.Expression) (common.EvaluationResult, error) {
	switch v := expression.(type) {
	case nil:
		return common.CallC(c, nil)
	case common.Boolean, common.Number, common.Character, common.String, common.Symbol, common.Pair, common.Syntax, common.Lambda:
		return common.CallC(c, v.(common.Datum))
	case common.Application:
		return common.EvalC(
			applicationProcedureEvaluated{
				c,
				v.Arguments,
			},
			v.Procedure,
		)
	case common.If:
		return common.EvalC(
			ifConditionEvaluated{
				c,
				v.Then,
				v.Else,
			},
			v.Condition,
		)
	case common.Let:
		return common.EvalC(
			letInitEvaluated{
				c,
				v.Variable,
				v.Body,
			},
			v.Init,
		)
	case common.Begin:
		return common.EvalC(
			beginFirstEvaluated{
				c,
				v.Expressions[1:],
			},
			v.Expressions[0],
		)
	case common.Set:
		return common.EvalC(
			setExpressionEvaluated{
				c,
				v.Variable,
			},
			v.Expression,
		)
	case common.Reference:
		return common.CallC(c, v.Variable.Value)
	default:
		if v == common.Void {
			return common.CallC(c, common.Void)
		}
		return nil, fmt.Errorf("evaluate: unhandled expression %#v", v)
	}
}

type escapeEvaluated struct{}

func (escapeEvaluated) Call(d common.Datum) (common.EvaluationResult, error) {
	return nil, fmt.Errorf("evaluate: escape continuation called without escaping context")
}

// perform further evaluation or call continuations as necessary until the nil continuation is called.
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
