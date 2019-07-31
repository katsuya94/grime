package common

import (
	"fmt"
	"reflect"
	"runtime"
	"strings"
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

func (d Pair) PrettyPrint(indent int) string {
	return fmt.Sprintf("(%v%v", PrettyPrint(d.First, indent), prettyPrintRest(d.Rest, indent+1))
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

func prettyPrintRest(d Datum, indent int) string {
	indentString := strings.Repeat(" ", indent)
	switch d := d.(type) {
	case Pair:
		return fmt.Sprintf("\n%v%v%v", indentString, PrettyPrint(d.First, indent), prettyPrintRest(d.Rest, indent))
	default:
		if d == Null {
			return ")"
		}
		return fmt.Sprintf("\n%v.\n%v%v)", indentString, indentString, Write(d))
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
	scopeList          scopeList
	marks              markSet
	phase              int
	sourceLocationTree *SourceLocationTree
}

func NewWrappedSyntax(d Datum, sourceLocationTree *SourceLocationTree) WrappedSyntax {
	return WrappedSyntax{
		datum:              d,
		scopeList:          scopeList{},
		sourceLocationTree: sourceLocationTree,
	}
}

func (d WrappedSyntax) Write() string {
	return fmt.Sprintf("#<syntax: %v>", Write(d.datum))
}

func (d WrappedSyntax) Datum() Datum {
	return d.datum
}

func (d WrappedSyntax) Push(scope *Scope, phase int) WrappedSyntax {
	d.scopeList = d.scopeList.push(scope, phase)
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

// Function represents a Grime function.
type Function struct {
	inner Expression
	argn  int
}

func NewFunction(inner Expression, argn int) Function {
	return Function{inner, argn}
}

func (d Function) Write() string {
	return "#<procedure>"
}

func (d Function) Call(c Continuation, args ...Datum) (Evaluation, error) {
	if len(args) != d.argn {
		return ErrorC(fmt.Errorf("evaluate: received %d arguments for procedure expecting %d", len(args), d.argn))
	}
	locations := make([]Location, len(args))
	for i, arg := range args {
		variable := NewVariable()
		variable.Set(arg)
		locations[i] = variable
	}
	ctx := NewEvaluationContext(locations...)
	return EvalC(ctx, c, d.inner)
}

// Native represents a Go function.
type Native struct {
	function func(Continuation, ...Datum) (Evaluation, error)
}

func NewNative(function func(Continuation, ...Datum) (Evaluation, error)) Native {
	return Native{function}
}

func (d Native) Write() string {
	i := interface{}(d.function)
	name := runtime.FuncForPC(reflect.ValueOf(i).Pointer()).Name()
	return fmt.Sprintf("#<go: %v>", name)
}

func (d Native) Call(c Continuation, args ...Datum) (Evaluation, error) {
	return d.function(c, args...)
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
