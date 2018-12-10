package common

import "fmt"

type Location interface{}

type Keyword struct {
	Transformer Procedure
}
type Variable struct {
	Value Datum
}

type PatternVariable struct {
	Match   interface{}
	Nesting int
}

type BindingSet map[int]map[Symbol]Location

type Identifier struct {
	Name  Symbol
	Marks int
}

// TODO: remove Environment

type Environment struct {
	leveled     BindingSet
	definitions map[Symbol]bool
	level       int
}

func NewEnvironment(init BindingSet) Environment {
	return Environment{
		cloneBindingSet(init),
		make(map[Symbol]bool),
		0,
	}
}

var EmptyEnvironment = NewEnvironment(make(BindingSet))

func (env Environment) Get(name Symbol) Location {
	locations, ok := env.leveled[env.level]
	if !ok {
		return nil
	}
	location, _ := locations[name]
	return location
}

func (env Environment) Define(name Symbol, levels []int, location Location) (Environment, error) {
	leveled := cloneBindingSet(env.leveled)
	for _, level := range levels {
		_, ok := leveled[level]
		if !ok {
			leveled[level] = make(map[Symbol]Location)
		}
		existing, ok := leveled[level][name]
		if ok && location != existing {
			return Environment{}, fmt.Errorf("previously defined at level %v: %v", level, name)
		}
		leveled[level][name] = location
	}
	return Environment{leveled, env.definitions, env.level}, nil
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
	_, ok := bindingSet[0]
	if !ok {
		bindingSet[0] = make(map[Symbol]Location)
	}
	return bindingSet
}

func (env Environment) Next() Environment {
	return Environment{env.leveled, env.definitions, env.level + 1}
}

func (env Environment) Clear() Environment {
	return Environment{env.leveled, make(map[Symbol]bool), env.level}
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

func cloneDefinitions(init map[Symbol]bool) map[Symbol]bool {
	definitions := make(map[Symbol]bool)
	for name := range init {
		definitions[name] = true
	}
	return definitions
}
