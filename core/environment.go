package core

type Binding interface{}

type Keyword struct {
	Transformer Datum
}
type Variable struct {
	Value Datum
}

type Environment struct {
	Bindings map[Symbol]Binding
}

func (e *Environment) Get(name Symbol) Binding {
	binding, _ := e.Bindings[name]
	return binding
}

func (e *Environment) Set(name Symbol, binding Binding) *Environment {
	e.Bindings[name] = binding
	return e
}
