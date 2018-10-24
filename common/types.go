package common

import (
	"fmt"
	"reflect"
	"runtime"
	"strings"
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
	Call(Continuation, ...Datum) (EvaluationResult, error)
}

type voidType struct{}

func (voidType) Write() string {
	return "#<void>"
}

func (v voidType) Debug() string {
	return Write(v)
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

type Syntax struct {
	Datum Datum
}

func (s Syntax) Write() string {
	return fmt.Sprintf("#<syntax: %v>", Write(s.Datum))
}

func (s Syntax) Debug() string {
	return fmt.Sprintf("#'%v", Write(s.Datum))
}

type Function func(Continuation, ...Datum) (EvaluationResult, error)

func (f Function) Write() string {
	var i interface{} = f
	name := runtime.FuncForPC(reflect.ValueOf(i).Pointer()).Name()
	return fmt.Sprintf("#<function: %v>", name)
}

func (f Function) Call(c Continuation, args ...Datum) (EvaluationResult, error) {
	return f(c, args...)
}

type Lambda struct {
	Variables []*Variable
	Body      Expression
}

func (Lambda) Write() string {
	return "#<lambda>"
}

func (l Lambda) Debug() string {
	var formals []string
	for _, variable := range l.Variables {
		formals = append(formals, fmt.Sprintf("%p", variable.Value))
	}
	return fmt.Sprintf("(lambda (%v) %v)", strings.Join(formals, " "), l.Body.Debug())
}

func (l Lambda) Call(c Continuation, args ...Datum) (EvaluationResult, error) {
	if len(args) != len(l.Variables) {
		return ErrorC(fmt.Errorf("wrong number of arguments %v for lambda expecting %v arguments", len(args), len(l.Variables)))
	}
	for i := range l.Variables {
		(*l.Variables[i]).Value = args[i]
	}
	return EvalC(c, l.Body)
}

type ContinuationProcedure struct {
	Continuation Continuation
}

func (ContinuationProcedure) Write() string {
	return "#<continuation>"
}

func (c ContinuationProcedure) Call(_ Continuation, args ...Datum) (EvaluationResult, error) {
	if len(args) != 1 {
		return ErrorC(fmt.Errorf("wrong number of arguments %v for continuation", len(args)))
	}
	return CallC(c.Continuation, args[0])
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
	Variable *Variable
	Init     Expression
	Body     Expression
}

func (l Let) Debug() string {
	return fmt.Sprintf("(let ((%p %v)) %v)", l.Variable.Value, l.Init.Debug(), l.Body.Debug())
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

type Define struct {
	Variable   *Variable
	Expression Expression
}

func (d Define) Debug() string {
	return fmt.Sprintf("(define %p %v)", d.Variable.Value, d.Expression.Debug())
}

type Set struct {
	Variable   *Variable
	Expression Expression
}

func (s Set) Debug() string {
	return fmt.Sprintf("(set! %p %v)", s.Variable.Value, s.Expression.Debug())
}

type Reference struct {
	Variable *Variable
}

func (r Reference) Debug() string {
	return fmt.Sprintf("%p", r.Variable.Value)
}
