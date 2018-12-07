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

// IsSyntax determines whether a Datum is a Syntax object.
func IsSyntax(d Datum) bool {
	switch d := d.(type) {
	case WrappedSyntax:
		return true
	case Pair:
		return IsSyntax(d.First) && IsSyntax(d.Rest)
	default:
		return d == Null
	}
}

type identifier struct {
	name  Symbol
	marks int
}

// WrappedSyntax represents syntax along with its lexical context.
type WrappedSyntax struct {
	lexicalSubstitutions map[identifier]Location
	phaseSubstitutions   []map[identifier]Location
	marks                int
	datum                Datum
}

func NewWrappedSyntax(d Datum) WrappedSyntax {
	return WrappedSyntax{make(map[identifier]Location), nil, 0, d}
}

func (d WrappedSyntax) Write() string {
	return fmt.Sprintf("#<syntax: %v>", Write(d.datum))
}

func (d WrappedSyntax) Datum() Datum {
	return d.datum
}

func (d WrappedSyntax) PushOnto(datum Datum) WrappedSyntax {
	return WrappedSyntax{d.lexicalSubstitutions, d.phaseSubstitutions, d.marks, datum}
}

func (d WrappedSyntax) IsIdentifier() bool {
	_, ok := d.datum.(Symbol)
	return ok
}

func (d WrappedSyntax) IdentifierAt(phase int) (Symbol, Location) {
	name := d.datum.(Symbol)
	location, _ := d.lexicalSubstitutions[identifier{name, d.marks}]
	if location != nil {
		return name, location
	}
	if phase < 0 || len(d.phaseSubstitutions) <= phase {
		return name, nil
	}
	substitutions := d.phaseSubstitutions[phase]
	if substitutions == nil {
		return name, nil
	}
	location, _ = substitutions[identifier{name, d.marks}]
	return name, location
}

func (d WrappedSyntax) IdentifierEquals(other WrappedSyntax) bool {
	return d.datum.(Symbol) == other.datum.(Symbol) && d.marks == other.marks
}

func (d WrappedSyntax) Unmarked() bool {
	return d.marks == 0
}

func (d WrappedSyntax) DefinedAt(phase int) []WrappedSyntax {
	if len(d.phaseSubstitutions) < phase+1 {
		return nil
	}
	if d.phaseSubstitutions[phase] == nil {
		return nil
	}
	var defined []WrappedSyntax
	for id, _ := range d.phaseSubstitutions[phase] {
		defined = append(defined, WrappedSyntax{d.lexicalSubstitutions, d.phaseSubstitutions, id.marks, id.name})
	}
	return defined
}

func (d WrappedSyntax) Set(name Symbol, location Location) WrappedSyntax {
	lexicalSubstitutions := make(map[identifier]Location, len(d.lexicalSubstitutions))
	for id, l := range d.lexicalSubstitutions {
		lexicalSubstitutions[id] = l
	}
	lexicalSubstitutions[identifier{name, d.marks}] = location
	return WrappedSyntax{lexicalSubstitutions, d.phaseSubstitutions, d.marks, d.datum}
}

func (d WrappedSyntax) SetAt(name Symbol, phase int, location Location) WrappedSyntax {
	n := len(d.phaseSubstitutions)
	if phase+1 > n {
		n = phase + 1
	}
	phaseSubstitutions := make([]map[identifier]Location, n)
	for i, substitutions := range d.phaseSubstitutions {
		phaseSubstitutions[i] = substitutions
	}
	if phase < len(d.phaseSubstitutions) {
		phaseSubstitutions[phase] = make(map[identifier]Location, len(d.phaseSubstitutions[phase])+1)
		for id, l := range d.phaseSubstitutions[phase] {
			phaseSubstitutions[phase][id] = l
		}
	} else {
		phaseSubstitutions[phase] = make(map[identifier]Location, 1)
	}
	phaseSubstitutions[phase][identifier{name, d.marks}] = location
	return WrappedSyntax{d.lexicalSubstitutions, phaseSubstitutions, d.marks, d.datum}
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
