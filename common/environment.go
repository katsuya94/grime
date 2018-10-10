package common

import "fmt"

type Location interface{}

type Keyword struct {
	Transformer Procedure
}
type Variable struct {
	Value   Datum
	Defined bool
}

type Binding struct {
	Location Location
	Levels   []int // nil for lexical bindings
}

type Environment struct {
	bindings    map[Symbol]Binding
	definitions map[Symbol]bool
	level       int
}

func NewEnvironment(bindings map[Symbol]Binding) Environment {
	return Environment{bindings, make(map[Symbol]bool), 0}
}

var EmptyEnvironment = NewEnvironment(make(map[Symbol]Binding))

func (env Environment) Bindings() map[Symbol]Binding {
	bindings := make(map[Symbol]Binding, len(env.bindings))
	for n, b := range env.bindings {
		bindings[n] = b
	}
	return bindings
}

func (env Environment) Definitions() map[Symbol]Binding {
	definitions := make(map[Symbol]Binding)
	for n := range env.definitions {
		binding := env.bindings[n]
		// Lexical bindings are exported at level 0.
		if binding.Levels == nil {
			binding.Levels = []int{0}
		}
		definitions[n] = binding
	}
	return definitions
}

func (env Environment) Get(name Symbol) Location {
	binding, _ := env.bindings[name]
	if binding.Levels == nil {
		return binding.Location
	}
	for _, level := range binding.Levels {
		if level == env.level {
			return binding.Location
		}
	}
	return nil
}

func (env Environment) Set(name Symbol, location Location) Environment {
	bindings := env.Bindings()
	bindings[name] = Binding{location, nil}
	return Environment{bindings, env.definitions, env.level}
}

func (env Environment) Define(name Symbol, levels []int, location Location) (Environment, error) {
	bindings := env.Bindings()
	bindings[name] = Binding{location, levels}
	if defined, _ := env.definitions[name]; defined {
		return env, fmt.Errorf("previously defined: %v", name)
	}
	definitions := make(map[Symbol]bool)
	for n, b := range env.definitions {
		definitions[n] = b
	}
	definitions[name] = true
	return Environment{bindings, env.definitions, env.level}, nil
}

func (env Environment) Next() Environment {
	return Environment{env.bindings, env.definitions, env.level + 1}
}

func (env Environment) Clear() Environment {
	return Environment{env.bindings, make(map[Symbol]bool), env.level}
}
