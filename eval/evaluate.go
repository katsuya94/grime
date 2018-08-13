package eval

import (
	"github.com/katsuya94/grime/core"
	"fmt"
)

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

func (e *Environment) Evaluate(datum core.Datum) (core.Datum, error) {
	switch v := datum.(type) {
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
			return nil, fmt.Errorf("eval: keyword %v not allowed in expression context", v)
		case Variable:
			return nil, fmt.Errorf("eval: variable evaluation not implemented")
		case nil:
			return nil, fmt.Errorf("eval: unbound identifier %v", v)
		default:
			return nil, fmt.Errorf("eval: unhandled binding %#v", b)
		}
	case core.Pair:
		if s, ok := v.First.(core.Symbol); ok {
			if _, ok := e.Get(s).(Keyword); ok {
				return nil, fmt.Errorf("eval: macro expansion not implemented")
			}
			switch s {
			case core.Symbol("define"):
				return nil, fmt.Errorf("eval: define not implemented")
			}
		}
		proc, err := e.Evaluate(v.First)
		if err != nil {
			return nil, err
		}
		var (
			rest = v.Rest
			args []core.Datum
		)
		for {
			if v, ok := rest.(core.Pair); ok {
				if arg, err := e.Evaluate(v.First); err == nil {
					args = append(args, arg)
					rest = v.Rest
				} else {
					return nil, err
				}
			} else if rest == nil {
				break
			} else {
				return nil, fmt.Errorf("eval: malformed application")
			}
		}
		return proc, nil // TODO
	case nil:
		return nil, fmt.Errorf("eval: empty application")
	default:
		return nil, fmt.Errorf("eval: unhandled datum %#v", v)
	}
}
