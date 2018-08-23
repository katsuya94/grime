package eval

import (
	"fmt"
	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/read"
)

func Errorf(format string, a ...interface{}) error {
	return fmt.Errorf("eval: "+format, a...)
}

var ErrImproperList = Errorf("improper list")

func EvaluateTopLevelProgram(env *common.Environment, topLevelProgram []common.Datum) error {
	expression, err := ExpandBody(env, topLevelProgram)
	if err != nil {
		return err
	}
	_, err = EvaluateExpression(env, expression)
	return err
}

func ExpandBody(env *common.Environment, forms []common.Datum) (common.Datum, error) {
	var (
		i           int
		definitions []common.Define
	)
	// Expand and handle definitions, deferring expansion of variable definitions.
	env = env.GetDefinitionContext()
	for i = 0; i < len(forms); i++ {
		form, err := ExpandMacro(env, forms[i])
		if err != nil {
			return nil, err
		}
		switch v := form.(type) {
		case common.DefineSyntax:
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
	env = env.GetExpressionContext()
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
	expression, err := ExpandMacro(env, common.Pair{common.Symbol("begin"), list(forms[i:]...)})
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

func ExpandMacro(env *common.Environment, syntax common.Datum) (common.Datum, error) {
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
	if result, ok, err := Match(syntax, PatternApplication, nil); err != nil {
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

func expandMacroMatching(env *common.Environment, syntax common.Datum, pattern common.Datum) (common.Datum, bool, error) {
	// TODO identifiers are actually wrapped
	result, ok, err := Match(syntax, pattern, nil)
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

func Unwrap(env *common.Environment, syntax common.Datum) (common.Datum, error) {
	// TODO unwrap, handling hygiene
	return syntax, nil
}

func EvaluateExpression(env *common.Environment, expression common.Datum) (common.Datum, error) {
	switch v := expression.(type) {
	case common.Boolean, common.Number, common.Character, common.String:
		return v, nil
	case common.Symbol:
		if binding := env.Get(v); binding == nil {
			return nil, Errorf("unbound identifier %v", v)
		} else {
			// No keywords should appear in an expanded expression.
			return binding.(common.Variable).Value, nil
		}
	case common.Application:
		procedure, err := EvaluateExpression(env, v.Procedure)
		if err != nil {
			return nil, err
		}
		var args []common.Datum
		for _, subexpression := range v.Arguments {
			if arg, err := EvaluateExpression(env, subexpression); err != nil {
				return nil, err
			} else {
				args = append(args, arg)
			}
		}
		return Apply(env, procedure, args...)
	case common.Quote:
		return v.Datum, nil
	case common.If:
		if condition, err := EvaluateExpression(env, v.Condition); err != nil {
			return nil, err
		} else if condition == common.Boolean(false) {
			return EvaluateExpression(env, v.Else)
		} else {
			return EvaluateExpression(env, v.Then)
		}
	case common.Let:
		value, err := EvaluateExpression(env, v.Value)
		if err != nil {
			return nil, err
		}
		return EvaluateExpression(env.Set(v.Name, common.Variable{value}), v.Body)
	case common.Begin:
		if len(v.Forms) < 1 {
			return nil, Errorf("begin: empty in expression context")
		}
		for _, subexpression := range v.Forms[:len(v.Forms)-1] {
			if _, err := EvaluateExpression(env, subexpression); err != nil {
				return nil, err
			}
		}
		if value, err := EvaluateExpression(env, v.Forms[len(v.Forms)-1]); err != nil {
			return nil, err
		} else {
			return value, nil
		}
	default:
		return nil, Errorf("unhandled expression %#v", v)
	}
}

func Apply(env *common.Environment, procedure common.Datum, args ...common.Datum) (common.Datum, error) {
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

func each(list common.Datum, fn func(common.Datum) error) error {
	for {
		if p, ok := list.(common.Pair); ok {
			if err := fn(p.First); err != nil {
				return err
			} else {
				list = p.Rest
			}
		} else if list == nil {
			return nil
		} else {
			return ErrImproperList
		}
	}
}

func list(data ...common.Datum) common.Datum {
	if len(data) == 0 {
		return nil
	} else {
		return common.Pair{data[0], list(data[1:]...)}
	}
}
