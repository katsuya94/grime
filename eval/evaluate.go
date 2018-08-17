package eval

import (
	"github.com/katsuya94/grime/core"
)

func Errorf(format string, a ...interface{}) error {
	return Errorf("eval: "+format, a...)
}

type Binding interface{}

type Keyword struct{}
type Variable struct{}

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
		switch b := e.Get(v).(type) {
		case Keyword:
			return nil, Errorf("eval: keyword %v not allowed in expression context", v)
		case Variable:
			return nil, Errorf("eval: variable evaluation not implemented")
		case nil:
			return nil, Errorf("eval: unbound identifier %v", v)
		default:
			return nil, Errorf("eval: unhandled binding %#v", b)
		}
	case core.Pair:
		if s, ok := v.First.(core.Symbol); ok {
			switch s {
			case core.Symbol("quote"):
				return quote(v.Rest)
			}
		}
		proc, err := e.EvaluateExpression(v.First)
		if err != nil {
			return nil, err
		}
		var (
			rest = v.Rest
			args []core.Datum
		)
		for {
			if v, ok := rest.(core.Pair); ok {
				if arg, err := e.EvaluateExpression(v.First); err == nil {
					args = append(args, arg)
					rest = v.Rest
				} else {
					return nil, err
				}
			} else if rest == nil {
				break
			} else {
				return nil, Errorf("eval: malformed application")
			}
		}
		return proc, nil // TODO
	case nil:
		return nil, Errorf("eval: empty application")
	default:
		return nil, Errorf("eval: unhandled expression %#v", v)
	}
}

func quote(rest core.Datum) (core.Datum, error) {
	if p, ok := rest.(core.Pair); !ok || p.Rest != nil {
		return nil, Errorf("quote: malformed")
	} else {
		return p.First, nil
	}
}

func (e *Environment) EvaluateBody(body []core.Datum) (core.Datum, error) {
	expressions := body
	if len(expressions) < 1 {
		return nil, Errorf("empty body")
	}
	for expression := range expressions[:len(body)-1] {
		if _, err := e.EvaluateExpression(expression); err != nil {
			return nil, err
		}
	}
	if val, err := e.EvaluateExpression(expressions[len(body)-1]); err != nil {
		return nil, err
	} else {
		return val, nil
	}
}

func (e *Environment) EvaluateTopLevelProgram(topLevelProgram []core.Datum) error {
	_, err := e.EvaluateBody(topLevelProgram)
	return err
}
