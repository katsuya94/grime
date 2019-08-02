package common

import "fmt"

type Location struct {
	value *Datum
}

func NewLocation() Location {
	value := Datum(nil)
	return Location{&value}
}

func (l Location) Get() Datum {
	return *l.value
}

func (l Location) Set(value Datum) {
	*l.value = value
}

func (l Location) String() string {
	return fmt.Sprintf("Location{%p}", l.value)
}
