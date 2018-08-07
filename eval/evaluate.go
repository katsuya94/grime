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

type Binding struct{}

type Environment struct{}

func NewEnvironment() *Environment {
	return &Environment{}
}

func (e *Environment) Get(s Symbol) *Binding {
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
		if b := e.Get(Symbol(v)); b != nil {
			return b, nil // TODO
		} else {
			return nil, fmt.Errorf("eval: unbound identifier %v", v)
		}
	case read.Pair:
		if s, ok := v.First.(read.Symbol); ok {
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
