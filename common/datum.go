package common

import "fmt"

type Datum interface{}
type Procedure interface {
	Datum
	Call(Environment, ...Datum) (EvaluationResult, error)
}

var Void = &struct{}{}

type Boolean bool

type Number string

type Character rune

type String string

type Symbol string

type Pair struct {
	First Datum
	Rest  Datum
}

type WrappedSyntax struct {
	Form Datum
	// TODO add marks and substitutions
}

type Function func(Environment, ...Datum) (EvaluationResult, error)

func (f Function) Call(env Environment, args ...Datum) (EvaluationResult, error) {
	return f(env, args...)
}

type Application struct {
	Procedure Datum
	Arguments []Datum
}

type Quote struct {
	Datum Datum
}

type If struct {
	Condition Datum
	Then      Datum
	Else      Datum
}

type Let struct {
	Name Symbol
	Init Datum
	Body Datum
}

type DefineSyntax struct {
	Name       Symbol
	Expression Datum
}

type Define struct {
	Name   Symbol
	Syntax Datum
}

type Begin struct {
	Forms []Datum
}

type Lambda struct {
	Formals []Symbol
	Body    Datum
}

type Closure struct {
	Lambda
	Bindings map[Symbol]Binding
}

func (c Closure) Call(env Environment, args ...Datum) (EvaluationResult, error) {
	if len(args) != len(c.Formals) {
		return ErrorC(fmt.Errorf("wrong number of arguments %v for closure expecting %v arguments", len(args), len(c.Formals)))
	}
	env = env.WithBindings(c.Bindings)
	for i := range c.Formals {
		env = env.Set(c.Formals[i], Variable{args[i]})
	}
	return EvalC(env, c.Body)
}

type LetSyntax struct{}
