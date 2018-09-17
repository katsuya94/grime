package eval

import (
	"fmt"

	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/read"
	"github.com/katsuya94/grime/util"
)

func Errorf(format string, a ...interface{}) error {
	return fmt.Errorf("eval: "+format, a...)
}

func ExpandBody(env common.Environment, forms []common.Datum) (common.Datum, error) {
	var (
		i           int
		definitions []common.Define
	)
	// Expand and handle definitions, deferring expansion of variable definitions.
	env = env.SetDefinitionContext()
	for i = 0; i < len(forms); i++ {
		form, err := ExpandMacro(env, forms[i])
		if err != nil {
			return nil, err
		}
		switch v := form.(type) {
		case common.DefineSyntax:
			// TODO pull phase n + 1 bindings from runtime via some sort of chain.
			if _, err := EvaluateExpression(env, v.Expression); err != nil {
				return nil, err
			} else {
				return nil, Errorf("define-syntax not implemented")
			}
		case common.Define:
			definitions = append(definitions, v)
			continue
		case common.Begin:
			forms = append(forms[0:i], append(v.Forms, forms[i+1:]...)...)
			i -= 1
			continue
		case common.LetSyntax:
			return nil, Errorf("let-syntax not implemented")
		}
		break
	}
	// Expand variable definitions.
	env = env.SetExpressionContext()
	var definitionNames []common.Symbol
	var definitionExpressions []common.Datum
	for _, definition := range definitions {
		expression, err := ExpandMacro(env, definition.Syntax)
		if err != nil {
			return nil, err
		}
		definitionNames = append(definitionNames, definition.Name)
		definitionExpressions = append(definitionExpressions, expression)
	}
	// Expand a begin with the remaining expressions.
	expression, err := ExpandMacro(env, common.Pair{common.Symbol("begin"), util.List(forms[i:]...)})
	if err != nil {
		return nil, err
	}
	// Wrap it in a letrec* with the definitions.
	for i := len(definitions) - 1; i >= 0; i-- {
		expression = common.Let{definitionNames[i], definitionExpressions[i], expression}
	}
	return expression, nil
}

var (
	PatternMacroUseList                = read.MustReadString("(keyword _ ...)")[0]
	PatternMacroUseImproperList        = read.MustReadString("(keyword _ ... . _)")[0]
	PatternMacroUseSingletonIdentifier = read.MustReadString("keyword")[0]
	PatternMacroUseSet                 = read.MustReadString("(set! keyword _)")[0]
	PatternApplication                 = read.MustReadString("(procedure arguments ...)")[0]
)

func ExpandMacro(env common.Environment, syntax common.Datum) (common.Datum, error) {
	// TODO use literals to ensure that set! would point at the keyword in base
	if expression, ok, err := expandMacroMatching(env, syntax, PatternMacroUseSet); err != nil {
		return nil, err
	} else if ok {
		return expression, nil
	}
	if expression, ok, err := expandMacroMatching(env, syntax, PatternMacroUseList); err != nil {
		return nil, err
	} else if ok {
		return expression, nil
	}
	if expression, ok, err := expandMacroMatching(env, syntax, PatternMacroUseImproperList); err != nil {
		return nil, err
	} else if ok {
		return expression, nil
	}
	if expression, ok, err := expandMacroMatching(env, syntax, PatternMacroUseSingletonIdentifier); err != nil {
		return nil, err
	} else if ok {
		return expression, nil
	}
	if result, ok, err := util.Match(syntax, PatternApplication, nil); err != nil {
		return nil, err
	} else if ok {
		var application common.Application
		if expression, err := ExpandMacro(env, result[common.Symbol("procedure")].(common.Datum)); err != nil {
			return nil, err
		} else {
			application.Procedure = expression
		}
		for _, argument := range result[common.Symbol("arguments")].([]interface{}) {
			if expression, err := ExpandMacro(env, argument.(common.Datum)); err != nil {
				return nil, err
			} else {
				application.Arguments = append(application.Arguments, expression)
			}
		}
		return application, nil
	}
	// TODO handle malformed applications. Note that the Racket implementation does not appear to consider () invalid
	return Unwrap(env, syntax)
}

func expandMacroMatching(env common.Environment, syntax common.Datum, pattern common.Datum) (common.Datum, bool, error) {
	// TODO identifiers are actually wrapped
	result, ok, err := util.Match(syntax, pattern, nil)
	if err != nil {
		return nil, false, err
	} else if !ok {
		return nil, false, nil
	}
	name, ok := result[common.Symbol("keyword")].(common.Symbol)
	if !ok {
		return nil, false, nil
	}
	binding := env.Get(name)
	if binding == nil {
		return nil, false, nil
	}
	keyword, ok := binding.(common.Keyword)
	if !ok {
		return nil, false, nil
	}
	output, err := CallWithEscape(func(escape common.Continuation) (common.EvaluationResult, error) {
		return Apply(env.SetContinuation(escape), keyword.Transformer, syntax)
	})
	if err != nil {
		return nil, false, err
	}
	expression, err := ExpandMacro(env, output)
	if err != nil {
		return nil, false, err
	}
	return expression, true, nil
}

func Unwrap(env common.Environment, syntax common.Datum) (common.Datum, error) {
	// TODO unwrap, handling hygiene
	return syntax, nil
}

func EvaluateExpression(env common.Environment, expression common.Datum) (common.EvaluationResult, error) {
	switch v := expression.(type) {
	case common.Boolean, common.Number, common.Character, common.String:
		return common.CallC(env, v)
	case common.Symbol:
		binding := env.Get(v)
		if binding == nil {
			return nil, Errorf("unbound identifier %v", v)
		}
		variable, ok := binding.(common.Variable)
		if !ok {
			return nil, Errorf("unexpected non-variable binding in expression context: %#v", binding)
		}
		return common.CallC(env, variable.Value)
	case common.Quote:
		return common.CallC(env, v.Datum)
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
				v.Forms[1:],
			}),
			v.Forms[0],
		)
	case common.Lambda:
		return common.CallC(
			env,
			common.Closure{v, env.Bindings()},
		)
	case common.Set:
		binding := env.Get(v.Variable)
		if binding == nil {
			return nil, Errorf("unbound identifier %v", v)
		}
		variable, ok := binding.(*common.Variable)
		if !ok {
			return nil, Errorf("unexpected non-variable binding in expression context: %#v", binding)
		}
		return common.CallC(
			env,
			common.Void,
		)
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

func EvaluateExpressionOnce(env common.Environment, expression common.Datum) (common.Datum, error) {
	return CallWithEscape(func(escape common.Continuation) (common.EvaluationResult, error) {
		return EvaluateExpression(env.SetContinuation(escape), expression)
	})
}
