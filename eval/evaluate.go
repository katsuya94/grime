package eval

import (
	"fmt"

	"github.com/katsuya94/grime/common"
)

func EvaluateExpression(c common.Continuation, expression common.Expression) (common.EvaluationResult, error) {
	switch v := expression.(type) {
	case nil:
		return common.CallC(c, nil)
	case common.Boolean, common.Number, common.Character, common.String, common.Symbol, common.Pair, common.Lambda, common.WrappedSyntax:
		return common.CallC(c, v.(common.Datum))
	case common.Application:
		return common.EvalC(
			applicationProcedureEvaluated{c, v.Arguments},
			v.Procedure,
		)
	case common.If:
		return common.EvalC(
			ifConditionEvaluated{c, v.Then, v.Else},
			v.Condition,
		)
	case common.Let:
		return common.EvalC(
			letInitEvaluated{c, v.Variable, v.Body},
			v.Init,
		)
	case common.Begin:
		return common.EvalC(
			beginFirstEvaluated{c, v.Expressions[1:]},
			v.Expressions[0],
		)
	case common.Define:
		return common.EvalC(
			defineExpressionEvaluated{c, v.Variable},
			v.Expression,
		)
	case common.Set:
		return common.EvalC(
			setExpressionEvaluated{c, v.Variable},
			v.Expression,
		)
	case common.Reference:
		if !v.Variable.Defined {
			return common.ErrorC(fmt.Errorf("evaluate: cannot reference identifier before its definition"))
		}
		return common.CallC(c, v.Variable.Value)
	case common.SyntaxCase:
		return common.EvalC(
			syntaxCaseInputEvaluated{c, v.Literals, v.Patterns, v.PatternVariableBindings, v.Fenders, v.Outputs},
			v.Input,
		)
	case common.SyntaxTemplate:
		datum, err := evaluateSyntaxTemplate(v.Template, []int{})
		if err != nil {
			return common.ErrorC(err)
		}
		return common.CallC(c, datum)
	default:
		if v == common.Void {
			return common.CallC(c, common.Void)
		}
		return common.ErrorC(fmt.Errorf("evaluate: unhandled expression %#v", v))
	}
}

func getTemplatePatternVariables(datum common.Datum) (map[*common.PatternVariable]interface{}, error) {
	switch datum := datum.(type) {
	case common.WrappedSyntax:
		return map[*common.PatternVariable]interface{}{}, nil
	case common.PatternVariableReference:
		return map[*common.PatternVariable]interface{}{datum.PatternVariable: datum.PatternVariable.Match}, nil
	case common.Pair:
		if first, ok := datum.First.(common.Subtemplate); ok {

		}
		first, err := getTemplatePatternVariables(datum.First, path)
		if err != nil {
			return nil, err
		}
		rest, err := getTemplatePatternVariables(datum.Rest, path)
		if err != nil {
			return nil, err
		}
		return common.Pair{first, rest}, nil
	default:
		return nil, fmt.Errorf("evaluate: unhandled syntax template %#v", datum)
	}
}

// New approach: calculate which pattern variables will determine the number at compile time. Then it will be easy to ensure that they match.
func evaluateSyntaxTemplate(datum common.Datum, path []int) (common.Datum, error) {
	switch datum := datum.(type) {
	case common.WrappedSyntax:
		return datum, nil
	case common.PatternVariableReference:
		syntax := datum.PatternVariable.Match.(common.WrappedSyntax)
		return syntax, nil
	case common.Pair:
		if first, ok := datum.First.(common.Subtemplate); ok {

		}
		first, err := evaluateSyntaxTemplate(datum.First, path)
		if err != nil {
			return nil, err
		}
		rest, err := evaluateSyntaxTemplate(datum.Rest, path)
		if err != nil {
			return nil, err
		}
		return common.Pair{first, rest}, nil
	default:
		return nil, fmt.Errorf("evaluate: unhandled syntax template %#v", datum)
	}
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
