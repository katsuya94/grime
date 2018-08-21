package core

type Binding interface{}

type Keyword struct {
	Transformer Datum
}
type Variable struct {
	Value Datum
}

type Environment struct {
	Bindings          map[Symbol]Binding
	ExpressionContext bool
}

func (e *Environment) Get(name Symbol) Binding {
	binding, _ := e.Bindings[name]
	return binding
}

func (e *Environment) Set(name Symbol, binding Binding) *Environment {
	e.Bindings[name] = binding
	return e
}

func (e *Environment) GetExpressionContext() *Environment {
	return &Environment{e.Bindings, true}
}

func (e *Environment) GetDefinitionContext() *Environment {
	return &Environment{e.Bindings, false}
}
