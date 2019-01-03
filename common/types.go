package common

import (
	"fmt"
	"reflect"
	"runtime"
)

type Datum interface{}

type Writable interface {
	Datum
	Write() string
}

// Procedure represents a callable value.
type Procedure interface {
	Datum
	Call(Continuation, ...Datum) (EvaluationResult, error)
}

// Apply attempts to call the given procedure if it is callable.
func Apply(c Continuation, procedureV Datum, argumentsV ...Datum) (EvaluationResult, error) {
	p, ok := procedureV.(Procedure)
	if !ok {
		return nil, fmt.Errorf("application: non-procedure in procedure position")
	}
	return p.Call(c, argumentsV...)
}

// Void is returned as the result of side-effects with no useful result.
var Void = &voidType{}

type voidType struct{}

func (voidType) Write() string {
	return "#<void>"
}

// Null is the empty list.
var Null = &nullType{}

type nullType struct{}

func (nullType) Write() string {
	return "()"
}

// Underscore is a value used in patterns as a non-capturing wildcard.
var Underscore = &underscoreType{}

type underscoreType struct{}

// Ellipsis is a value used in patterns to signify repitition.
var Ellipsis = &ellipsisType{}

type ellipsisType struct{}

// Boolean represents a boolean value.
type Boolean bool

func (d Boolean) Write() string {
	if d {
		return "#t"
	} else {
		return "#f"
	}
}

// Number represents a numerical value.
type Number string

func (d Number) Write() string {
	return string(d)
}

// Character represents a unicode code point.
type Character rune

func (d Character) Write() string {
	return fmt.Sprintf(`#\%v`, string(d))
}

// String represents a string.
type String string

func (d String) Write() string {
	return fmt.Sprintf(`"%v"`, string(d))
}

// Symbol represents a symbolic value.
type Symbol string

func (d Symbol) Write() string {
	return string(d)
}

// Pair represents a pair of data.
type Pair struct {
	First Datum
	Rest  Datum
}

func (d Pair) Write() string {
	return fmt.Sprintf("(%v%v", Write(d.First), formatRest(d.Rest))
}

func formatRest(d Datum) string {
	switch d := d.(type) {
	case Pair:
		return fmt.Sprintf(" %v%v", Write(d.First), formatRest(d.Rest))
	default:
		if d == Null {
			return ")"
		}
		return fmt.Sprintf(" . %v)", Write(d))
	}
}

// Function represents a Go function.
type Function func(Continuation, ...Datum) (EvaluationResult, error)

func (f Function) Write() string {
	var i interface{} = f
	name := runtime.FuncForPC(reflect.ValueOf(i).Pointer()).Name()
	return fmt.Sprintf("#<function: %v>", name)
}

func (f Function) Call(c Continuation, args ...Datum) (EvaluationResult, error) {
	return f(c, args...)
}

// Lambda represents a Grime function.
type Lambda struct {
	Variables []*Variable
	Body      Expression
}

func (Lambda) Write() string {
	return "#<lambda>"
}

func (d Lambda) Call(c Continuation, args ...Datum) (EvaluationResult, error) {
	if len(args) != len(d.Variables) {
		return ErrorC(fmt.Errorf("wrong number of arguments %v for lambda expecting %v arguments", len(args), len(d.Variables)))
	}
	for i := range d.Variables {
		(*d.Variables[i]).Value = args[i]
	}
	return EvalC(c, d.Body)
}

// ContinuationProcedure represents a callable continuation.
type ContinuationProcedure struct {
	Continuation Continuation
}

func (ContinuationProcedure) Write() string {
	return "#<continuation>"
}

func (d ContinuationProcedure) Call(_ Continuation, args ...Datum) (EvaluationResult, error) {
	if len(args) != 1 {
		return ErrorC(fmt.Errorf("wrong number of arguments %v for continuation", len(args)))
	}
	return CallC(d.Continuation, args[0])
}
