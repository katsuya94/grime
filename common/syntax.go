package common

import "fmt"

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
	sourceLocationTree   *SourceLocationTree
}

func NewWrappedSyntax(d Datum, sourceLocationTree *SourceLocationTree) WrappedSyntax {
	return WrappedSyntax{make(map[identifier]Location), nil, 0, d, sourceLocationTree}
}

// TODO: delete
func (d WrappedSyntax) LexicalSubstitutions() string {
	s := ""
	for id := range d.lexicalSubstitutions {
		s += fmt.Sprintf("%v@%v\n", id.name, id.marks)
	}
	return s
}

func (d WrappedSyntax) Write() string {
	return fmt.Sprintf("#<syntax: %v>", Write(d.datum))
}

func (d WrappedSyntax) Datum() Datum {
	return d.datum
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

func (d WrappedSyntax) PushDown() Datum {
	switch datum := d.datum.(type) {
	case Pair:
		var (
			firstSourceLocationTree *SourceLocationTree
			restSourceLocationTree  *SourceLocationTree
		)
		if d.sourceLocationTree != nil {
			first := d.sourceLocationTree.Children.(Pair).First.(SourceLocationTree)
			firstSourceLocationTree = &first
			rest := d.sourceLocationTree.Children.(Pair).Rest.(SourceLocationTree)
			restSourceLocationTree = &rest
		}
		return Pair{d.pushOnto(datum.First, firstSourceLocationTree), d.pushOnto(datum.Rest, restSourceLocationTree)}
	default:
		panic(fmt.Sprintf("unhandled syntax #<%T>", datum))
	}
}

func (d WrappedSyntax) pushOnto(datum Datum, sourceLocationTree *SourceLocationTree) WrappedSyntax {
	return WrappedSyntax{d.lexicalSubstitutions, d.phaseSubstitutions, d.marks, datum, sourceLocationTree}
}

func (d WrappedSyntax) IsIdentifier() bool {
	_, ok := d.datum.(Symbol)
	return ok
}

func (d WrappedSyntax) IdentifierName() Symbol {
	return d.datum.(Symbol)
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

func (d WrappedSyntax) Phases() []int {
	name := d.datum.(Symbol)
	var phases []int
	for phase, substitutions := range d.phaseSubstitutions {
		_, ok := substitutions[identifier{name, d.marks}]
		if ok {
			phases = append(phases, phase)
		}
	}
	return phases
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
		defined = append(defined, WrappedSyntax{d.lexicalSubstitutions, d.phaseSubstitutions, id.marks, id.name, nil})
	}
	return defined
}

func (d WrappedSyntax) GetAt(name Symbol, phase int) Location {
	_, location := d.pushOnto(name, nil).IdentifierAt(phase)
	return location
}

func (d WrappedSyntax) Set(name Symbol, location Location) WrappedSyntax {
	lexicalSubstitutions := make(map[identifier]Location, len(d.lexicalSubstitutions))
	for id, l := range d.lexicalSubstitutions {
		lexicalSubstitutions[id] = l
	}
	lexicalSubstitutions[identifier{name, d.marks}] = location
	return WrappedSyntax{lexicalSubstitutions, d.phaseSubstitutions, d.marks, d.datum, d.sourceLocationTree}
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
	return WrappedSyntax{d.lexicalSubstitutions, phaseSubstitutions, d.marks, d.datum, d.sourceLocationTree}
}
