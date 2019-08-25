package common

import "fmt"

type Location struct {
	value *interface{}
}

func NewLocation() Location {
	value := interface{}(nil)
	return Location{&value}
}

func (l Location) Get() interface{} {
	return *l.value
}

func (l Location) Set(value interface{}) {
	*l.value = value
}

func (l Location) String() string {
	return fmt.Sprintf("Location{%p}", l.value)
}
