package common

import (
	"fmt"
	"reflect"
	"runtime"
)

type Datum interface{}

type Writable interface {
	Write() string
}

type Expression interface {
	Debug() string
}

type Procedure interface {
	Datum
	Call(Environment, ...Datum) (EvaluationResult, error)
}

type voidType struct{}

func (voidType) Write() string {
	return "#<void>"
}

var Void = &voidType{}

type Boolean bool

func (b Boolean) Write() string {
	if b {
		return "#t"
	} else {
		return "#f"
	}
}

func (b Boolean) Debug() string {
	return Write(b)
}

type Number string

func (n Number) Write() string {
	return string(n)
}

func (n Number) Debug() string {
	return Write(n)
}

type Character rune

func (c Character) Write() string {
	return fmt.Sprintf(`#\%v`, string(c))
}

func (c Character) Debug() string {
	return Write(c)
}

type String string

func (s String) Write() string {
	return fmt.Sprintf(`"%v"`, string(s))
}

func (s String) Debug() string {
	return Write(s)
}

type Symbol string

func (s Symbol) Write() string {
	return string(s)
}

func (s Symbol) Debug() string {
	return fmt.Sprintf("'%v", Write(s))
}

type Pair struct {
	First Datum
	Rest  Datum
}

func (p Pair) Write() string {
	return fmt.Sprintf("(%v%v", Write(p.First), formatRest(p.Rest))
}

func formatRest(d Datum) string {
	switch d := d.(type) {
	case Pair:
		return fmt.Sprintf(" %v%v", Write(d.First), formatRest(d.Rest))
	case nil:
		return ")"
	default:
		return fmt.Sprintf(" . %v)", Write(d))
	}
}

func (p Pair) Debug() string {
	return fmt.Sprintf("'%v", Write(p))
}

type Function func(Environment, ...Datum) (EvaluationResult, error)

func (f Function) Write() string {
	var i interface{} = f
	name := runtime.FuncForPC(reflect.ValueOf(i).Pointer()).Name()
	return fmt.Sprintf("#<func: %v>", name)
}

func (f Function) Call(env Environment, args ...Datum) (EvaluationResult, error) {
	return f(env, args...)
}

type Closure struct {
	Lambda
	Bindings map[Symbol]Binding
}

func (Closure) Write() string {
	return "#<closure>"
}

func (c Closure) Call(env Environment, args ...Datum) (EvaluationResult, error) {
	if len(args) != len(c.Formals) {
		return ErrorC(fmt.Errorf("wrong number of arguments %v for closure expecting %v arguments", len(args), len(c.Formals)))
	}
	env = env.WithBindings(c.Bindings)
	for i := range c.Formals {
		env = env.Set(c.Formals[i], &Variable{args[i]})
	}
	return EvalC(env, c.Body)
}

type ContinuationProcedure struct {
	Continuation Continuation
}

func (ContinuationProcedure) Write() string {
	return "#<continuation>"
}

func (c ContinuationProcedure) Call(env Environment, args ...Datum) (EvaluationResult, error) {
	if len(args) != 1 {
		return ErrorC(fmt.Errorf("wrong number of arguments %v for continuation", len(args)))
	}
	return c.Continuation.Call(args[0])
}

type Application struct {
	Procedure Expression
	Arguments []Expression
}

func (a Application) Debug() string {
	s := fmt.Sprintf("(%v", a.Procedure.Debug())
	for _, argument := range a.Arguments {
		s = fmt.Sprintf("%v %v", s, argument.Debug())
	}
	return fmt.Sprintf("%v)", s)
}

type If struct {
	Condition Expression
	Then      Expression
	Else      Expression
}

func (i If) Debug() string {
	return fmt.Sprintf("(if %v %v %v)", i.Condition.Debug(), i.Then.Debug(), i.Else.Debug())
}

type Let struct {
	Name Symbol
	Init Expression
	Body Expression
}

func (l Let) Debug() string {
	return fmt.Sprintf("(let ((%v %v)) %v)", l.Name, l.Init.Debug(), l.Body.Debug())
}

type Begin struct {
	Expressions []Expression
}

func (b Begin) Debug() string {
	s := "(begin"
	for _, expression := range b.Expressions {
		s = fmt.Sprintf("%v %v", s, expression.Debug())
	}
	return fmt.Sprintf("%v)", s)
}

type Lambda struct {
	Formals []Symbol
	Body    Expression
}

func (l Lambda) Debug() string {
	s := "(lambda ("
	for i, formal := range l.Formals {
		if i == 0 {
			s = fmt.Sprintf("%v%v", s, formal)
		} else {
			s = fmt.Sprintf("%v %v", s, formal)
		}
	}
	return fmt.Sprintf("%v) %v)", s, l.Body.Debug())
}

type Set struct {
	Name       Symbol
	Expression Expression
}

func (s Set) Debug() string {
	return fmt.Sprintf("(set! %v %v)", s.Name, s.Expression.Debug())
}

type Reference struct {
	Name Symbol
}

func (r Reference) Debug() string {
	return string(r.Name)
}
