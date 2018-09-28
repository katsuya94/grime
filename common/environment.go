package common

type Binding interface{}

type Keyword struct {
	Transformer Procedure
}
type Variable struct {
	Value Datum
}

type Environment struct {
	bindings map[Symbol]Binding
	next     *Environment
}

func NewEnvironment(bindings map[Symbol]Binding) Environment {
	return Environment{bindings, nil}
}

var EmptyEnvironment = NewEnvironment(make(map[Symbol]Binding))

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
	return Environment{bindings, env.next}
}

func (env Environment) Next() Environment {
	if env.next == nil {
		return EmptyEnvironment
	} else {
		return *env.next
	}
}

func (env Environment) SetNext(next Environment) Environment {
	return Environment{env.bindings, &next}
}
