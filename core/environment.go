package core

type Binding interface{}

type Keyword Datum
type Variable Datum

type Environment struct {
	Bindings map[Symbol]Binding
}

func (e *Environment) Get(s Symbol) Binding {
	binding, _ := e.Bindings[s]
	return binding
}
