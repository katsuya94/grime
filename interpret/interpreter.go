package interpret

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

type Environment struct{}

func NewEnvironment() *Environment {
	return &Environment{}
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
		// TODO
		return Symbol(v), nil
	case read.Pair:
		// TODO
		return Pair{v.First, v.Rest}, nil
	default:
		return nil, fmt.Errorf("interpret: unhandled datum %#v", v)
	}
}
