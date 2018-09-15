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
	// TODO use literals to ensure that set! would point at the procedure in base
	if expression, ok, err := expandMacroMatching(env, syntax, PatternMacroUseSet); err != nil {
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
	output, err := Apply(env, keyword.Transformer, syntax)
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

type expressionEvaluated struct {
	called *bool
	value  *common.Datum
}

func newExpressionEvaluated() expressionEvaluated {
	var (
		called bool
		value  common.Datum
	)
	return expressionEvaluated{&called, &value}
}

func (c expressionEvaluated) Call(d common.Datum) (common.EvaluationResult, error) {
	if *c.called {
		return nil, fmt.Errorf("eval: continuation called twice in non-reentrant context")
	}
	*c.called = true
	*c.value = d
	return nil, nil
}

func EvaluateExpressionOnce(env common.Environment, expression common.Datum) (common.Datum, error) {
	continuation := newExpressionEvaluated()
	env = env.SetContinuation(continuation)
	for {
		evaluationResult, err := EvaluateExpression(env, expression)
		if err != nil {
			return nil, err
		}
		for {
			furtherEvaluation := false
			switch v := evaluationResult.(type) {
			case common.FurtherEvaluation:
				env = v.Environment
				expression = v.Expression
				furtherEvaluation = true
			case common.ContinuationCall:
				evaluationResult, err = v.Continuation.Call(v.Value)
				if err != nil {
					return nil, err
				} else if *continuation.called {
					return *continuation.value, nil
				}
			default:
				return nil, fmt.Errorf("eval: unhandled evaluation result: %#v", v)
			}
			if furtherEvaluation {
				break
			}
		}
	}
}

func EvaluateExpression(env common.Environment, expression common.Datum) (common.EvaluationResult, error) {
	switch v := expression.(type) {
	case common.Boolean, common.Number, common.Character, common.String:
		return common.ContinuationCall{env.Continuation(), v}, nil
	case common.Symbol:
		if binding := env.Get(v); binding == nil {
			return nil, Errorf("unbound identifier %v", v)
		} else {
			// No keywords should appear in an expanded expression.
			value := binding.(common.Variable).Value
			return common.ContinuationCall{env.Continuation(), value}, nil
		}
	case common.Quote:
		return common.ContinuationCall{env.Continuation(), v.Datum}, nil
	case common.Application:
		return common.FurtherEvaluation{
			env.SetContinuation(applicationProcedureEvaluated{
				env,
				v.Arguments,
			}),
			v.Procedure,
		}, nil
	case common.If:
		return common.FurtherEvaluation{
			env.SetContinuation(ifConditionEvaluated{
				env,
				v.Then,
				v.Else,
			}),
			v.Condition,
		}, nil
	case common.Let:
		return common.FurtherEvaluation{
			env.SetContinuation(letInitEvaluated{
				env,
				v.Name,
				v.Body,
			}),
			v.Init,
		}, nil
	case common.Begin:
		return common.FurtherEvaluation{
			env.SetContinuation(beginFirstEvaluated{
				env,
				v.Forms[1:],
			}),
			v.Forms[0],
		}, nil
	default:
		return nil, Errorf("unhandled expression %#v", v)
	}
}

func Apply(env common.Environment, procedure common.Datum, args ...common.Datum) (common.Datum, error) {
	if p, ok := procedure.(common.Procedure); ok {
		if value, err := p(env, args...); err != nil {
			return nil, err
		} else {
			return value, nil
		}
	} else {
		return nil, Errorf("application: non-procedure in procedure position")
	}
}
