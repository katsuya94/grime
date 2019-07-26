package common

import (
	"fmt"
)

type Binding int

var nextBinding = Binding(0)

func NewBinding() Binding {
	binding := nextBinding
	nextBinding++
	return binding
}

func (b Binding) String() string {
	return fmt.Sprintf("%#8x", int(b))
}
