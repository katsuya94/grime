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
	first Value
	rest  Value
}

type Environment struct{}

func (e *Environment) Evaluate(datum read.Datum) (Value, error) {
	switch v := datum.(type) {
	case Boolean:
		return Boolean(v), nil
	case Number:
		return Number(v), nil
	case Character:
		return Character(v), nil
	case String:
		return String(v), nil
	case Symbol:
		// TODO
		return Symbol(v), nil
	case Pair:
		// TODO
		return Pair(v), nil
	default:
		return nil, fmt.Errorf("interpret: unhandled datum %#v", v)
	}
}
