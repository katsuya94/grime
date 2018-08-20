package eval

import (
	"fmt"
	"github.com/katsuya94/grime/core"
	"github.com/katsuya94/grime/match"
	"github.com/katsuya94/grime/read"
)

func Errorf(format string, a ...interface{}) error {
	return fmt.Errorf("eval: "+format, a...)
}

var ErrImproperList = Errorf("improper list")

func EvaluateTopLevelProgram(env *core.Environment, topLevelProgram []core.Datum) error {
	expression, err := ExpandBody(env, topLevelProgram)
	if err != nil {
		return err
	}
	_, err = EvaluateExpression(env, expression)
	return err
}

func ExpandBody(env *core.Environment, forms []core.Datum) (core.Datum, error) {
	var definitions []core.Definition
	var (
		i    int
		form core.Datum
	)
	// Expand and handle definitions, deferring expansion of variable definitions.
	for i, form = range forms {
		form, err := ExpandMacro(env, form)
		if err != nil {
			return nil, err
		}
		switch v := form.(type) {
		case core.SyntaxDefinition:
			if _, err := EvaluateExpression(env, v.Form); err != nil {
				return nil, err
			} else {
				return nil, Errorf("define-syntax not implemented")
			}
		case core.Definition:
			definitions = append(definitions, v)
		case core.Begin:
			forms = append(forms[0:i], append(v.Forms, forms[i+1:]...)...)
			i -= 1
		case core.LetSyntax:
			return nil, Errorf("let-syntax not implemented")
		}
		break
	}
	// Expand variable definitions.
	definitionMap := make(map[core.Symbol]core.Datum)
	for _, definition := range definitions {
		form, err := ExpandMacro(env, definition.Form)
		if err != nil {
			return nil, err
		}
		definitionMap[definition.Name] = form
	}
	// Expand expressions.
	var expressions []core.Datum
	for _, form = range forms[i:] {
		form, err := ExpandMacro(env, form)
		if err != nil {
			return nil, err
		}
		expressions = append(expressions, form)
	}
	// TODO Not sure if this NewLetRecStar should expand the body instead.
	return NewLetrecStar(definitionMap, expressions), nil
}

func NewLetrecStar(definitionMap map[core.Symbol]core.Datum, forms []core.Datum) core.Datum {
	return core.Begin{forms}
}

var (
	PatternMacroUseList                = read.MustReadString("(keyword _ ...)")[0]
	PatternMacroUseImproperList        = read.MustReadString("(keyword _ ... . _)")[0]
	PatternMacroUseSingletonIdentifier = read.MustReadString("keyword")[0]
	PatternMacroUseSet                 = read.MustReadString("(set! keyword _)")[0]
	PatternApplication                 = read.MustReadString("(procedure arguments ...)")[0]
)

func ExpandMacro(env *core.Environment, syntax core.Datum) (core.Datum, error) {
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
	if result, ok, err := match.Match(syntax, PatternApplication, nil); err != nil {
		return nil, err
	} else if ok {
		var application core.Application
		if expression, err := ExpandMacro(env, result[core.Symbol("procedure")].(core.Datum)); err != nil {
			return nil, err
		} else {
			application.Procedure = expression
		}
		for _, argument := range result[core.Symbol("arguments")].([]interface{}) {
			if expression, err := ExpandMacro(env, argument.(core.Datum)); err != nil {
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


func expandMacroMatching(env *core.Environment, syntax core.Datum, pattern core.Datum) (core.Datum, bool, error) {
	// TODO identifiers are actually wrapped
	result, ok, err := match.Match(syntax, pattern, nil)
	if err != nil {
		return nil, false, err
	} else if !ok {
		return nil, false, nil
	}
	name, ok := result[core.Symbol("keyword")].(core.Symbol)
	if !ok {
		return nil, false, nil
	}
	binding := env.Get(name)
	if binding == nil {
		return nil, false, nil
	}
	keyword, ok := binding.(core.Keyword)
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

func Unwrap(env *core.Environment, syntax core.Datum) (core.Datum, error) {
	// TODO unwrap, handling hygiene
	return syntax, nil
}

func EvaluateExpression(env *core.Environment, expression core.Datum) (core.Datum, error) {
	switch v := expression.(type) {
	case core.Boolean, core.Number, core.Character, core.String:
		return v, nil
	case core.Symbol:
		if binding := env.Get(v); binding == nil {
			return nil, Errorf("unbound identifier %v", v)
		} else {
			// No keywords should appear in an expanded expression.
			return binding.(core.Variable).Value, nil
		}
	case core.Application:
		procedure, err := EvaluateExpression(env, v.Procedure)
		if err != nil {
			return nil, err
		}
		var args []core.Datum
		for _, subexpression := range v.Arguments {
			if arg, err := EvaluateExpression(env, subexpression); err != nil {
				return nil, err
			} else {
				args = append(args, arg)
			}
		}
		return Apply(env, procedure, args...)
	case core.Quote:
		return v.Datum, nil
	case core.If:
		if condition, err := EvaluateExpression(env, v.Condition); err != nil {
			return nil, err
		} else if condition == core.Boolean(false) {
			return EvaluateExpression(env, v.Else)
		} else {
			return EvaluateExpression(env, v.Then)
		}
	case core.Let:
		value, err := EvaluateExpression(env, v.Value)
		if err != nil {
			return nil, err
		}
		return EvaluateExpression(env.Set(v.Name, core.Variable{value}), v.Body)
	case core.Begin:
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

func Apply(env *core.Environment, procedure core.Datum, args ...core.Datum) (core.Datum, error) {
	if p, ok := procedure.(core.Procedure); ok {
		if value, err := p(env, args...); err != nil {
			return nil, err
		} else {
			return value, nil
		}
	} else {
		return nil, Errorf("application: non-procedure in procedure position")
	}
}

func Each(list core.Datum, fn func(core.Datum) error) error {
	for {
		if p, ok := list.(core.Pair); ok {
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
