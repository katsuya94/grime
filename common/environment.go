package common

type Binding interface{}

type Keyword struct {
	Transformer Datum
}
type Variable struct {
	Value Datum
}

type Environment struct {
	bindings          map[Symbol]Binding
	continuation      Continuation
	expressionContext bool
}

func NewEnvironment(bindings map[Symbol]Binding, continuation Continuation) Environment {
	return Environment{bindings, continuation, false}
}

func (env Environment) Get(name Symbol) Binding {
	binding, _ := env.bindings[name]
	return binding
}

func (env Environment) Set(name Symbol, binding Binding) Environment {
	newEnv := Environment{
		make(map[Symbol]Binding, len(env.bindings)),
		env.continuation,
		env.expressionContext,
	}
	for n, b := range env.bindings {
		newEnv.bindings[n] = b
	}
	newEnv.bindings[name] = binding
	return newEnv
}

func (env Environment) Empty() Environment {
	return Environment{
		make(map[Symbol]Binding),
		env.continuation,
		env.expressionContext,
	}
}

func (env Environment) Continuation() Continuation {
	return env.continuation
}

func (env Environment) SetContinuation(c Continuation) Environment {
	return Environment{env.bindings, c, env.expressionContext}
}

func (env Environment) ExpressionContext() bool {
	return env.expressionContext
}

func (env Environment) SetExpressionContext() Environment {
	return Environment{env.bindings, env.continuation, true}
}

func (env Environment) SetDefinitionContext() Environment {
	return Environment{env.bindings, env.continuation, false}
}
