package common

type Binding interface{}

type Keyword struct {
	Transformer Datum
}
type Variable struct {
	Value Datum
}

type Environment struct {
	bindings     map[Symbol]Binding
	continuation Continuation
}

func NewEnvironment(bindings map[Symbol]Binding, continuation Continuation) Environment {
	return Environment{bindings, continuation}
}

func (env Environment) Bindings() map[Symbol]Binding {
	bindings := make(map[Symbol]Binding, len(env.bindings))
	for n, b := range env.bindings {
		bindings[n] = b
	}
	return bindings
}

func (env Environment) Get(name Symbol) Binding {
	binding, _ := env.bindings[name]
	return binding
}

func (env Environment) Set(name Symbol, binding Binding) Environment {
	bindings := env.Bindings()
	bindings[name] = binding
	return Environment{bindings, env.continuation}
}

func (env Environment) Empty() Environment {
	return Environment{make(map[Symbol]Binding), env.continuation}
}

func (env Environment) WithBindings(bindings map[Symbol]Binding) Environment {
	return Environment{bindings, env.continuation}
}

func (env Environment) Continuation() Continuation {
	return env.continuation
}

func (env Environment) SetContinuation(c Continuation) Environment {
	return Environment{env.bindings, c}
}
