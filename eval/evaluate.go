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
		bindings := make(map[*common.PatternVariable]interface{}, len(v.PatternVariables))
		for _, patternVariable := range v.PatternVariables {
			bindings[patternVariable] = patternVariable.Match
		}
		datum, err := evaluateSyntaxTemplate(v.Template, bindings)
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

func evaluateSyntaxTemplate(datum common.Datum, bindings map[*common.PatternVariable]interface{}) (common.Datum, error) {
	switch datum := datum.(type) {
	case common.WrappedSyntax:
		return datum, nil
	case common.PatternVariableReference:
		return bindings[datum.PatternVariable].(common.Datum), nil
	case common.Pair:
		if first, ok := datum.First.(common.Subtemplate); ok {
			data, err := evaluateSubtemplate(first, bindings)
			if err != nil {
				return nil, err
			}
			rest, err := evaluateSyntaxTemplate(datum.Rest, bindings)
			if err != nil {
				return nil, err
			}
			result := rest
			for i := len(data) - 1; i >= 0; i-- {
				result = common.Pair{data[i], result}
			}
			return result, nil
		}
		first, err := evaluateSyntaxTemplate(datum.First, bindings)
		if err != nil {
			return nil, err
		}
		rest, err := evaluateSyntaxTemplate(datum.Rest, bindings)
		if err != nil {
			return nil, err
		}
		return common.Pair{first, rest}, nil
	default:
		return nil, fmt.Errorf("evaluate: unhandled syntax template %#v", datum)
	}
}

func evaluateSubtemplate(subtemplate common.Subtemplate, bindings map[*common.PatternVariable]interface{}) ([]common.Datum, error) {
	if subtemplate.Nesting == 0 {
		datum, err := evaluateSyntaxTemplate(subtemplate.Subtemplate, bindings)
		if err != nil {
			return nil, err
		}
		return []common.Datum{datum}, nil
	}
	n := len(bindings[subtemplate.PatternVariables[0]].([]interface{}))
	for _, patternVariable := range subtemplate.PatternVariables {
		if len(bindings[patternVariable].([]interface{})) != n {
			return nil, fmt.Errorf("evaluate: differing number of matches for syntax template")
		}
	}
	var data []common.Datum
	for j := 0; j > n; j++ {
		nestedBindings := make(map[*common.PatternVariable]interface{}, len(bindings))
		for patternVariable, match := range bindings {
			nestedBindings[patternVariable] = match
		}
		for _, patternVariable := range subtemplate.PatternVariables {
			nestedBindings[patternVariable] = nestedBindings[patternVariable].([]interface{})[j]
		}
		nestedData, err := evaluateSubtemplate(common.Subtemplate{subtemplate.Subtemplate, subtemplate.Nesting - 1, subtemplate.PatternVariables}, nestedBindings)
		if err != nil {
			return nil, err
		}
		data = append(data, nestedData...)
	}
	return data, nil
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
