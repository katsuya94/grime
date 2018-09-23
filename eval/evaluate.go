package eval

import (
	"fmt"

	"github.com/katsuya94/grime/common"
)

func Errorf(format string, a ...interface{}) error {
	return fmt.Errorf("eval: "+format, a...)
}

func EvaluateExpression(env common.Environment, expression common.Expression) (common.EvaluationResult, error) {
	switch v := expression.(type) {
	case nil:
		return common.CallC(env, nil)
	case common.Boolean, common.Number, common.Character, common.String, common.Symbol, common.Pair:
		return common.CallC(env, v.(common.Datum))
	case common.Application:
		return common.EvalC(
			env.SetContinuation(applicationProcedureEvaluated{
				env,
				v.Arguments,
			}),
			v.Procedure,
		)
	case common.If:
		return common.EvalC(
			env.SetContinuation(ifConditionEvaluated{
				env,
				v.Then,
				v.Else,
			}),
			v.Condition,
		)
	case common.Let:
		return common.EvalC(
			env.SetContinuation(letInitEvaluated{
				env,
				v.Name,
				v.Body,
			}),
			v.Init,
		)
	case common.Begin:
		return common.EvalC(
			env.SetContinuation(beginFirstEvaluated{
				env,
				v.Expressions[1:],
			}),
			v.Expressions[0],
		)
	case common.Lambda:
		return common.CallC(
			env,
			common.Closure{v, env.Bindings()},
		)
	case common.Set:
		binding := env.Get(v.Name)
		if binding == nil {
			return nil, Errorf("unbound identifier %v", v.Name)
		}
		variable, ok := binding.(*common.Variable)
		if !ok {
			return nil, Errorf("unexpected non-variable binding in expression context: %#v", binding)
		}
		return common.EvalC(
			env.SetContinuation(setExpressionEvaluated{
				env,
				variable,
			}),
			v.Expression,
		)
	case common.Reference:
		binding := env.Get(v.Name)
		if binding == nil {
			return nil, Errorf("unbound identifier %v", v.Name)
		}
		variable, ok := binding.(*common.Variable)
		if !ok {
			return nil, Errorf("unexpected non-variable binding in expression context: %#v", binding)
		}
		return common.CallC(env, variable.Value)
	default:
		return nil, Errorf("unhandled expression %#v", v)
	}
}

type escapeEvaluated struct{}

func (escapeEvaluated) Call(d common.Datum) (common.EvaluationResult, error) {
	return nil, Errorf("escape continuation called without escaping context")
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
			evaluationResult, err = EvaluateExpression(v.Environment, v.Expression)
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
			return nil, Errorf("unhandled evaluation result: %#v", v)
		}
	}
}

func EvaluateExpressionOnce(env common.Environment, expression common.Expression) (common.Datum, error) {
	return CallWithEscape(func(escape common.Continuation) (common.EvaluationResult, error) {
		return EvaluateExpression(env.SetContinuation(escape), expression)
	})
}
