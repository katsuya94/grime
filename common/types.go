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
	Evaluate(Continuation) (EvaluationResult, error)
}

type Procedure interface {
	Datum
	Call(Continuation, ...Datum) (EvaluationResult, error)
}

// Void is returned as the result of side-effects with no useful result.
var Void = &voidType{}

type voidType struct{}

func (voidType) Write() string {
	return "#<void>"
}

func (voidType) Evaluate(c Continuation) (EvaluationResult, error) {
	return CallC(c, Void)
}

// Underscore is a value used in patterns as a non-capturing wildcard.
var Underscore = &underscoreType{}

// UnderscoreKeyword cannot be expanded, but is used to identify usages of Underscore.
var UnderscoreKeyword = &Keyword{Function(func(Continuation, ...Datum) (EvaluationResult, error) {
	return nil, fmt.Errorf("cannot expand underscore")
})}

type underscoreType struct{}

// Ellipsis is a value used in patterns to signify repitition.
var Ellipsis = &ellipsisType{}

// EllipsisKeyword cannot be expanded, but is used to identify usages of Ellipsis, as well as repition in syntax templates.
var EllipsisKeyword = &Keyword{Function(func(Continuation, ...Datum) (EvaluationResult, error) {
	return nil, fmt.Errorf("cannot expand ellipsis")
})}

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

func (d Boolean) Evaluate(c Continuation) (EvaluationResult, error) {
	return CallC(c, d)
}

// Number represents a numerical value.
type Number string

func (d Number) Write() string {
	return string(d)
}

func (d Number) Evaluate(c Continuation) (EvaluationResult, error) {
	return CallC(c, d)
}

// Character represents a unicode code point.
type Character rune

func (d Character) Write() string {
	return fmt.Sprintf(`#\%v`, string(d))
}

func (d Character) Evaluate(c Continuation) (EvaluationResult, error) {
	return CallC(c, d)
}

// String represents a string.
type String string

func (d String) Write() string {
	return fmt.Sprintf(`"%v"`, string(d))
}

func (d String) Evaluate(c Continuation) (EvaluationResult, error) {
	return CallC(c, d)
}

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
	case nil:
		return ")"
	default:
		return fmt.Sprintf(" . %v)", Write(d))
	}
}

func (d Pair) Evaluate(c Continuation) (EvaluationResult, error) {
	return CallC(c, d)
}

// WrappedSyntax represents syntax along with its lexical context.
type WrappedSyntax struct {
	substitutions map[identifier]Location
	marks         int
	datum         Datum
}

func (d WrappedSyntax) Write() string {
	return fmt.Sprintf("#<syntax: %v>", Write(d.datum))
}

func IsSyntax(d Datum) bool {
	switch d := d.(type) {
	case WrappedSyntax:
		return true
	case Pair:
		return IsSyntax(d.First) && IsSyntax(d.Rest)
	case nil:
		return true
	default:
		return false
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

func (d Lambda) Evaluate(c Continuation) (EvaluationResult, error) {
	return CallC(c, d)
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

// Subtemplate represents the repitition of a SyntaxTemplate.
type Subtemplate struct {
	Subtemplate      SyntaxTemplate
	Nesting          int
	PatternVariables []*PatternVariable
}

// PatternVariableReference represents a reference to a pattern variable.
type PatternVariableReference struct {
	PatternVariable *PatternVariable
}
