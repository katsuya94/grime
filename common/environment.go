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

type level struct {
	level   int
	lexical bool
}

type Environment struct {
	bindings    map[Symbol]map[level]Location
	definitions map[Symbol]map[int]bool
	level       int
}

func NewEnvironment(init map[Symbol]map[int]Location) Environment {
	bindings := make(map[Symbol]map[level]Location)
	for name, locations := range init {
		bindings[name] = make(map[level]Location)
		for l, location := range locations {
			bindings[name][level{l, false}] = location
		}
	}
	return Environment{bindings, make(map[Symbol]map[int]bool), 0}
}

var EmptyEnvironment = NewEnvironment(make(map[Symbol]map[int]Location))

func (env Environment) cloneBindings() map[Symbol]map[level]Location {
	bindings := make(map[Symbol]map[level]Location, len(env.bindings))
	for name, locations := range env.bindings {
		bindings[name] = make(map[level]Location, len(env.bindings[name]))
		for level, location := range locations {
			bindings[name][level] = location
		}
	}
	return bindings
}

func (env Environment) Definitions() map[Symbol]map[int]Location {
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
	locations, _ := env.bindings[name]
	if locations == nil {
		return nil
	}
	if location, _ := locations[level{0, true}]; location != nil {
		return location
	}
	location, _ := locations[level{env.level, false}]
	return location
}

func (env Environment) Set(name Symbol, location Location) Environment {
	bindings := env.cloneBindings()
	if locations, _ := bindings[name]; locations == nil {
		bindings[name] = make(map[level]Location)
	}
	bindings[name][level{0, true}] = location
	return Environment{bindings, env.definitions, env.level}
}

func (env Environment) Define(name Symbol, levels []int, location Location) (Environment, error) {
	definitions, _ := env.definitions[name]
	if levels == nil {
		levels = []int{0}
	}
	for _, l := range levels {
		if definitions != nil {
			for x, _ := range definitions {
				if l == x {
					return Environment{}, fmt.Errorf("previously defined at level %v: %v", x, name)
				}
			}
		}
	}
	bindings := env.cloneBindings()
	newDefinitions := make(map[Symbol]map[int]bool, len(env.definitions))
	for name, definitions := range env.definitions {
		newDefinitions[name] = make(map[int]bool, len(definitions))
		for level := range definitions {
			newDefinitions[name][level] = true
		}
	}
	if locations, _ := bindings[name]; locations == nil {
		bindings[name] = make(map[level]Location)
	}
	if definitions, _ := newDefinitions[name]; definitions == nil {
		newDefinitions[name] = make(map[int]bool)
	}
	if levels == nil {
		bindings[name][level{0, true}] = Binding{location, levels}
		newDefinitions[name][l] = true
	} else {
		for _, l := range levels {
			bindings[name][level{l, false}] = Binding{location, levels}
			newDefinitions[name][l] = true
		}
	}
	return Environment{bindings, newDefinitions, env.level}, nil
}

func (env Environment) MustDefine(name Symbol, levels []int, location Location) Environment {
	env, err := env.Define(name, levels, location)
	if err != nil {
		panic(err.Error())
	}
	return env
}

func (env Environment) Next() Environment {
	return Environment{env.bindings, env.definitions, env.level + 1}
}

func (env Environment) Clear() Environment {
	return Environment{env.bindings, make(map[Symbol]map[int]bool), env.level}
}
