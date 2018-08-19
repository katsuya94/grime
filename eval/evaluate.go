package eval

import (
	"github.com/katsuya94/grime/core"
	"fmt"
	"github.com/katsuya94/grime/read"
)

func Errorf(format string, a ...interface{}) error {
	return fmt.Errorf("eval: "+format, a...)
}

var ErrImproperList = Errorf("improper list")

type Binding interface{}

type Keyword struct {
	transformer core.Datum
}
type Variable struct {
	value core.Datum
}

type Environment struct{}

func NewEnvironment() *Environment {
	return &Environment{}
}

func (e *Environment) Get(s core.Symbol) Binding {
	return nil
}

func (e *Environment) EvaluateExpression(expression core.Datum) (core.Datum, error) {
	switch v := expression.(type) {
	case core.Boolean:
		return v, nil
	case core.Number:
		return v, nil
	case core.Character:
		return v, nil
	case core.String:
		return v, nil
	case core.Symbol:
		if binding := e.Get(v); binding == nil {
			return nil, Errorf("unbound identifier %v", v)
		} else {
			_ := binding.(Variable) // No keywords should appear in an expanded expression
			return nil, Errorf("variable evaluation not implemented")
		}
	case core.Pair:
		return v, nil
	case nil:
		return nil, Errorf("empty procedure application")
	default:
		return nil, Errorf("unhandled expression %#v", v)
	}
}

func (e *Environment) let(rest core.Datum) (core.Datum, error) {
	return nil, Errorf("let: not implemented")
}

func quote(rest core.Datum) (core.Datum, error) {
	if p, ok := rest.(core.Pair); !ok || p.Rest != nil {
		return nil, Errorf("quote: malformed")
	} else {
		return p.First, nil
	}
}

func each(list core.Datum, fn func(core.Datum) error) error {
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

func (e *Environment) EvaluateBody(forms []core.Datum) (core.Datum, error) {
	var definitions []core.Definition
	var (
		i    int
		form core.Datum
	)
	// Expand and handle definitions, deferring expansion of variable definitions
	for i, form = range forms {
		form, err := e.ExpandMacro(form)
		if err != nil {
			return nil, err
		}
		switch v := form.(type) {
		case core.SyntaxDefinition:
			if _, err := e.EvaluateExpression(v.Form); err != nil {
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
	// Expand variable definitions
	definitionMap := make(map[core.Symbol]core.Datum)
	for _, definition := range definitions {
		form, err := e.ExpandMacro(definition.Form)
		if err != nil {
			return nil, err
		}
		definitionMap[definition.Name] = form
	}
	// Expand expressions
	var expressions []core.Datum
	for _, form = range forms[i:] {
		form, err := e.ExpandMacro(form)
		if err != nil {
			return nil, err
		}
		expressions = append(expressions, form)
	}
	// TODO Not sure if this NewLetRecStar should expand the body instead.
	return e.EvaluateExpression(NewLetrecStar(definitionMap, expressions))
}

var (
	PatternMacroUseList = read.MustReadString("(keyword _ ...)")
	PatternMacroUseImproperList = read.MustReadString("(keyword _ ... . _)")
	PatternMacroUseSingletonIdentifier = read.MustReadString("keyword")
	PatternMacroUseSet = read.MustReadString("(set! keyword _)")
	PatternApplication = read.MustReadString("(procedure arguments ...)") // TODO use literals to ensure that set! would point at the procedure in base
)

func (e *Environment) ExpandMacro(syntax core.Datum) (core.Datum, error) {
	// TODO identifiers are actually wrapped
	if result, ok, err := Match(syntax, PatternMacroUseList, nil); err != nil {
		return nil, err
	} else if ok {
		if symbol, ok := result[core.Symbol("keyword")].(core.Symbol); ok {
			return e.expandMacro(symbol, syntax)
		}
	}
	if result, ok, err := Match(syntax, PatternMacroUseImproperList, nil); err != nil {
		return nil, err
	} else if ok {
		if symbol, ok := result[core.Symbol("keyword")].(core.Symbol); ok {
			return e.expandMacro(symbol, syntax)
		}
	}
	if result, ok, err := Match(syntax, PatternMacroUseSingletonIdentifier, nil); err != nil {
		return nil, err
	} else if ok {
		if symbol, ok := result[core.Symbol("keyword")].(core.Symbol); ok {
			return e.expandMacro(symbol, syntax)
		}
	}
	// TODO use literals to ensure that set! would point at the procedure in base
	if result, ok, err := Match(syntax, PatternMacroUseSet, nil); err != nil {
		return nil, err
	} else if ok {
		if symbol, ok := result[core.Symbol("keyword")].(core.Symbol); ok {
			return e.expandMacro(symbol, syntax)
		}
	}
	if result, ok, err := Match(syntax, PatternApplication, nil); err != nil {
		return nil, err
	} else if ok {
		var application core.Application
		if procedure, err := e.ExpandMacro(result[core.Symbol("procedure")]); err != nil {
			return nil, err
		} else {
			application.Procedure = procedure
		}
		err = each(result[core.Symbol("arguments")], func(d core.Datum) error {
			if arg, err := e.ExpandMacro(result[core.Symbol("procedure")]); err != nil {
				return err
			} else {
				application.Arguments = append(application.Arguments, arg)
				return nil
			}
		})
		if err != nil {
			return nil, err
		}
		return application, nil
	}
	// TODO handle malformed applications. Note that the Racket implementation does not appear to consider () invalid
	return e.Unwrap(syntax)
}

func (e *Environment) expandMacro(name core.Symbol, syntax core.Datum) (core.Datum, error) {
	if binding := e.Get(name); binding == nil {
		return e.Unwrap(syntax)
	} else if keyword, ok := binding.(Keyword); ok {
		if output, err := e.EvaluateExpression(core.Application{keyword.transformer, []core.Datum{syntax}}); err != nil {
			return nil, err
		} else {
			return e.ExpandMacro(output)
		}
	}
}

func (e *Environment) Unwrap(syntax core.Datum) (core.Datum, error) {
	// TODO unwrap, handling hygiene
	return syntax, nil
}

func (e *Environment) EvaluateTopLevelProgram(topLevelProgram []core.Datum) error {
	_, err := e.EvaluateBody(topLevelProgram)
	return err
}
