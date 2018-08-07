package eval

import (
	"github.com/katsuya94/grime/read"
	"fmt"
)

type Value interface{}

type Boolean bool
type Number string
type Character rune
type String string
type Symbol string
type Pair struct {
	First Value
	Rest  Value
}

type Binding interface{}

type Keyword struct{}
type Variable struct{}

type Environment struct{}

func NewEnvironment() *Environment {
	return &Environment{}
}

func (e *Environment) Get(s Symbol) Binding {
	return nil
}

func (e *Environment) Evaluate(datum read.Datum) (Value, error) {
	switch v := datum.(type) {
	case read.Boolean:
		return Boolean(v), nil
	case read.Number:
		return Number(v), nil
	case read.Character:
		return Character(v), nil
	case read.String:
		return String(v), nil
	case read.Symbol:
		switch b := e.Get(Symbol(v)).(type) {
		case Keyword:
			return nil, fmt.Errorf("eval: keyword %v not allowed in expression context", v)
		case Variable:
			return nil, fmt.Errorf("eval: variable evaluation not implemented")
		case nil:
			return nil, fmt.Errorf("eval: unbound identifier %v", v)
		default:
			return nil, fmt.Errorf("eval: unhandled binding %#v", b)
		}
	case read.Pair:
		if s, ok := v.First.(read.Symbol); ok {
			if _, ok := e.Get(Symbol(s)).(Keyword); ok {
				return nil, fmt.Errorf("eval: macro expansion not implemented")
			}
			switch Symbol(s) {
			case Symbol("define"):
				return nil, fmt.Errorf("eval: define not implemented")
			}
		}
		proc, err := e.Evaluate(v.First)
		if err != nil {
			return nil, err
		}
		var (
			rest = v.Rest
			args []Value
		)
		for {
			if v, ok := rest.(read.Pair); ok {
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
