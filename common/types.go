package common

import (
	"fmt"
	"reflect"
	"runtime"
)

type Datum interface{}

// Procedure represents a callable value.
type Procedure interface {
	Datum
	Call(Continuation, ...Datum) (Evaluation, error)
}

// Apply attempts to call the given procedure if it is callable.
func Apply(c Continuation, procedureV Datum, argumentsV ...Datum) (Evaluation, error) {
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

func (nullType) Mark(m *M) Marker {
	return Null
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

func (d Pair) Mark(m *M) Marker {
	return Pair{
		NewSyntax(d.First).Mark(m).Datum(),
		NewSyntax(d.Rest).Mark(m).Datum(),
	}
}

// WrappedSyntax represents syntax along with its lexical context.
type WrappedSyntax struct {
	datum              Datum
	scopeList          *ScopeList
	marks              markSet
	phase              int
	sourceLocationTree *SourceLocationTree
}

func NewWrappedSyntax(d Datum, sourceLocationTree *SourceLocationTree) WrappedSyntax {
	return WrappedSyntax{
		datum:              d,
		scopeList:          NewScopeList(),
		sourceLocationTree: sourceLocationTree,
	}
}

func (d WrappedSyntax) Write() string {
	return fmt.Sprintf("#<syntax: %v>", Write(d.datum))
}

func (d WrappedSyntax) Datum() Datum {
	return d.datum
}

func (d WrappedSyntax) Push(scope Scope, phase int) WrappedSyntax {
	d.scopeList = d.scopeList.Push(scope, phase)
	return d
}

func (d WrappedSyntax) Next() WrappedSyntax {
	d.phase++
	return d
}

func (d WrappedSyntax) Mark(m *M) Marker {
	d.marks = d.marks.xor(m)
	return d
}

func (d WrappedSyntax) Identifier() (Identifier, bool) {
	if _, ok := d.datum.(Symbol); !ok {
		return Identifier{}, false
	}
	return Identifier{d}, true
}

func (d WrappedSyntax) SourceLocation() SourceLocation {
	if d.sourceLocationTree == nil {
		return SourceLocation{}
	}
	return d.sourceLocationTree.SourceLocation
}

func (d WrappedSyntax) SourceLocationTree() *SourceLocationTree {
	return d.sourceLocationTree
}

func (d WrappedSyntax) PushOnto(datum Datum, sourceLocationTree *SourceLocationTree) WrappedSyntax {
	d.datum = datum
	d.sourceLocationTree = sourceLocationTree
	return d
}

// Function represents a Go function.
type Function func(Continuation, ...Datum) (Evaluation, error)

func (f Function) Write() string {
	var i interface{} = f
	name := runtime.FuncForPC(reflect.ValueOf(i).Pointer()).Name()
	return fmt.Sprintf("#<function: %v>", name)
}

func (f Function) Call(c Continuation, args ...Datum) (Evaluation, error) {
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

func (d Lambda) Call(c Continuation, args ...Datum) (Evaluation, error) {
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

func (d ContinuationProcedure) Call(_ Continuation, args ...Datum) (Evaluation, error) {
	if len(args) != 1 {
		return ErrorC(fmt.Errorf("wrong number of arguments %v for continuation", len(args)))
	}
	return CallC(d.Continuation, args[0])
}
