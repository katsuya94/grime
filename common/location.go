package common

import "fmt"

type Location interface {
	String() string
}

type Variable struct {
	value *Datum
}

func NewVariable() Variable {
	value := Datum(nil)
	return Variable{&value}
}

func (l Variable) Get() Datum {
	return *l.value
}

func (l Variable) Set(value Datum) {
	*l.value = value
}

func (l Variable) String() string {
	return fmt.Sprintf("Variable{%p}", l.value)
}
