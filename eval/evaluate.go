package eval

import (
	"github.com/katsuya94/grime/core"
	"fmt"
)

func Errorf(format string, a ...interface{}) error {
	return fmt.Errorf("eval: "+format, a...)
}

var ErrImproperList = Errorf("improper list")

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

func (e *Environment) EvaluateCoreForm(coreForm core.Datum) (core.Datum, error) {
	switch v := coreForm.(type) {
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
			return nil, Errorf("keyword %v not allowed in expression context", v)
		case Variable:
			return nil, Errorf("variable evaluation not implemented")
		case nil:
			return nil, Errorf("unbound identifier %v", v)
		default:
			return nil, Errorf("unhandled binding %#v", b)
		}
	case core.Pair:
		var subforms []core.Datum
		err := each(v, func(d core.Datum) error {
			if arg, err := e.EvaluateCoreForm(v.First); err != nil {
				return err
			} else {
				subforms = append(subforms, arg)
				return nil
			}
		})
		if err == ErrImproperList {
			return nil, Errorf("malformed procedure application")
		} else if err != nil {
			return nil, err
		}
		return nil, Errorf("procedure application not implemented")
	case nil:
		return nil, Errorf("empty procedure application")
	default:
		return nil, Errorf("unhandled core form %#v", v)
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

func (e *Environment) EvaluateBody(bodyForms []core.Datum) (core.Datum, error) {
	expressions := bodyForms
	if len(expressions) < 1 {
		return nil, Errorf("empty body")
	}
	for expression := range expressions[:len(bodyForms)-1] {
		if _, err := e.EvaluateCoreForm(expression); err != nil {
			return nil, err
		}
	}
	if val, err := e.EvaluateCoreForm(expressions[len(bodyForms)-1]); err != nil {
		return nil, err
	} else {
		return val, nil
	}
}

func (e *Environment) Expand(syntaxes []core.Datum) ([]core.Datum, error) {
	var definitions []core.Datum
	for _, syntax := range(syntaxes) {
		switch v := syntax.(type) {
		case core.Boolean:

		}
	}
}

func (e *Environment) EvaluateTopLevelProgram(topLevelProgram []core.Datum) error {
	_, err := e.EvaluateBody(topLevelProgram)
	return err
}
