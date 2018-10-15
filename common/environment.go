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

type BindingSet map[int]map[Symbol]Location

type Environment struct {
	leveled     BindingSet
	lexical     map[Symbol]Location
	definitions map[Symbol]bool
	level       int
}

func NewEnvironment(init BindingSet) Environment {
	return Environment{
		cloneBindingSet(init),
		make(map[Symbol]Location),
		make(map[Symbol]bool),
		0,
	}
}

var EmptyEnvironment = NewEnvironment(make(BindingSet))

func (env Environment) Get(name Symbol) Location {
	location, _ := env.lexical[name]
	if location != nil {
		return location
	}
	location, _ = env.leveled[env.level][name]
	return location
}

func (env Environment) Set(name Symbol, location Location) Environment {
	lexical := cloneLexical(env.lexical)
	lexical[name] = location
	return Environment{env.leveled, lexical, env.definitions, env.level}
}

func (env Environment) Define(name Symbol, levels []int, location Location) (Environment, error) {
	if levels == nil {
		if env.definitions[name] {
			return Environment{}, fmt.Errorf("previously defined: %v", name)
		}
		env = env.Set(name, location)
		definitions := cloneDefinitions(env.definitions)
		definitions[name] = true
		return Environment{env.leveled, env.lexical, definitions, env.level}, nil
	}
	leveled := cloneBindingSet(env.leveled)
	for _, level := range levels {
		existing, ok := env.leveled[level][name]
		if ok && location != existing {
			return Environment{}, fmt.Errorf("previously defined at level %v: %v", level, name)
		}
		leveled[level][name] = location
	}
	return Environment{leveled, env.lexical, env.definitions, env.level}, nil
}

func (env Environment) MustDefine(name Symbol, levels []int, location Location) Environment {
	env, err := env.Define(name, levels, location)
	if err != nil {
		panic(err.Error())
	}
	return env
}

func (env Environment) Bindings() BindingSet {
	bindingSet := cloneBindingSet(env.leveled)
	for name, location := range env.lexical {
		bindingSet[0][name] = location
	}
	return bindingSet
}

func (env Environment) Next() Environment {
	return Environment{env.leveled, env.lexical, env.definitions, env.level + 1}
}

func (env Environment) Clear() Environment {
	return Environment{env.leveled, env.lexical, make(map[Symbol]bool), env.level}
}

func cloneBindingSet(init BindingSet) BindingSet {
	bindingSet := make(BindingSet)
	for level, locations := range init {
		bindingSet[level] = make(map[Symbol]Location)
		for name, location := range locations {
			bindingSet[level][name] = location
		}
	}
	return bindingSet
}

func cloneLexical(init map[Symbol]Location) map[Symbol]Location {
	lexical := make(map[Symbol]Location)
	for name, location := range init {
		lexical[name] = location
	}
	return lexical
}

func cloneDefinitions(init map[Symbol]bool) map[Symbol]bool {
	definitions := make(map[Symbol]bool)
	for name := range init {
		definitions[name] = true
	}
	return definitions
}
