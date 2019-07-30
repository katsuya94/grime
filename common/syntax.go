package common

import (
	"fmt"
	"reflect"
	"strings"
)

// Identifier wraps a wrapped symbol (an identifier), providing access to its binding.
type Identifier struct {
	WrappedSyntax // TODO: this should be private
}

func NewIdentifier(name Symbol) Identifier {
	return Identifier{NewWrappedSyntax(name, nil)}
}

func (id Identifier) Name() Symbol {
	return id.datum.(Symbol)
}

func (id Identifier) Binding() (Binding, bool) {
	return id.scopeList.lookup(id, id.phase)
}

func (id Identifier) Role(env Environment) Role {
	binding, ok := id.Binding()
	if !ok {
		return nil
	}
	return env.Lookup(binding)
}

func (id Identifier) Mark(m *M) Identifier {
	return Identifier{NewSyntax(id.WrappedSyntax).Mark(m).Datum().(WrappedSyntax)}
}

func (id Identifier) BoundEqual(other Identifier) bool {
	return id.Name() == other.Name() && id.marks.equal(other.marks)
}

func (id Identifier) FreeEqual(other Identifier) bool {
	// TODO: what's the phase for free-equal?
	idBinding, idOk := id.Binding()
	otherBinding, otherOk := other.Binding()
	if !idOk && !otherOk {
		return id.Name() == other.Name()
	} else if idOk && otherOk {
		return idBinding == otherBinding
	} else {
		return false
	}
}

func (id Identifier) Binds(other Identifier) bool {
	if other.Name() != id.Name() {
		return false
	}
	return id.marks.equal(other.marks)
}

func DuplicateIdentifiers(ids ...Identifier) bool {
	markSetsByName := map[Symbol][]markSet{}
	for _, id := range ids {
		if _, ok := markSetsByName[id.Name()]; !ok {
			markSetsByName[id.Name()] = []markSet{}
		}
		markSetsByName[id.Name()] = append(markSetsByName[id.Name()], id.marks)
	}
	for _, markSets := range markSetsByName {
		if duplicateMarkSets(markSets...) {
			return true
		}
	}
	return false
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

// Syntax wraps syntax objects, providing a shared interface for both wrapped and unwrapped syntax objects.
type Syntax struct {
	datum Datum
}

func NewSyntax(datum Datum) Syntax {
	return Syntax{datum}
}

func (s Syntax) Equal(other Syntax) bool {
	if pair, ok := s.Pair(); ok {
		if otherPair, otherOk := other.Pair(); ok && otherOk {
			return Syntax{pair.First}.Equal(Syntax{otherPair.First}) && Syntax{pair.Rest}.Equal(Syntax{otherPair.Rest})
		} else if ok != otherOk {
			return false
		}
	}
	if id, ok := s.Identifier(); ok {
		if otherId, otherOk := other.Identifier(); ok && otherOk {
			return id.FreeEqual(otherId)
		} else if ok != otherOk {
			return false
		}
	}
	wrapped, ok := s.datum.(WrappedSyntax)
	if !ok {
		panic(fmt.Sprintf("not syntax: %v", Write(s.datum)))
	}
	otherWrapped, ok := other.datum.(WrappedSyntax)
	if !ok {
		panic(fmt.Sprintf("not syntax: %v", Write(other.datum)))
	}
	return reflect.DeepEqual(wrapped.Datum(), otherWrapped.Datum())
}

func (s Syntax) Push(scope *Scope, phase int) Syntax {
	switch d := s.datum.(type) {
	case WrappedSyntax:
		return Syntax{d.Push(scope, phase)}
	case Pair:
		return Syntax{
			Pair{
				Syntax{d.First}.Push(scope, phase).datum,
				Syntax{d.Rest}.Push(scope, phase).datum,
			},
		}
	default:
		// TODO: is the Null case necessary if list endings are usually wrapped?
		if d == Null {
			return Syntax{Null}
		}
		panic(fmt.Sprintf("unhandled syntax: %v", Write(d)))
	}
}

func (s Syntax) Next() Syntax {
	switch d := s.datum.(type) {
	case WrappedSyntax:
		return Syntax{d.Next()}
	case Pair:
		return Syntax{
			Pair{
				Syntax{d.First}.Next().datum,
				Syntax{d.Rest}.Next().datum,
			},
		}
	default:
		if d == Null {
			return Syntax{Null}
		}
		panic(fmt.Sprintf("unhandled syntax: %v", Write(d)))
	}
}

func (s Syntax) Identifier() (Identifier, bool) {
	wrapped, ok := s.datum.(WrappedSyntax)
	if !ok {
		return Identifier{}, false
	}
	return wrapped.Identifier()
}

func (s Syntax) IdentifierOrDie() Identifier {
	id, ok := s.Identifier()
	if !ok {
		panic("not an identifier")
	}
	return id
}

// TODO: remove this or generalize it for partially wrapped syntax
func (s Syntax) Unwrap() Datum {
	datum := s.datum
	if wrapped, ok := datum.(WrappedSyntax); ok {
		datum = wrapped.Datum()
	}
	return datum
}

func (s Syntax) Mark(m *M) Syntax {
	return Syntax{s.datum.(Marker).Mark(m)}
}

func (s Syntax) Datum() Datum {
	return s.datum
}

func (s Syntax) Pair() (Pair, bool) {
	switch d := s.datum.(type) {
	case WrappedSyntax:
		pair, ok := d.Datum().(Pair)
		if !ok {
			return Pair{}, false
		}
		var firstSourceLocationTree, restSourceLocationTree *SourceLocationTree
		if d.SourceLocationTree() != nil {
			firstSourceLocationTree = new(SourceLocationTree)
			*firstSourceLocationTree = d.SourceLocationTree().Children.(Pair).First.(SourceLocationTree)
			restSourceLocationTree = new(SourceLocationTree)
			*restSourceLocationTree = d.SourceLocationTree().Children.(Pair).Rest.(SourceLocationTree)
		}
		return Pair{
			d.PushOnto(pair.First, firstSourceLocationTree),
			d.PushOnto(pair.Rest, restSourceLocationTree),
		}, true
	case Pair:
		return d, true
	default:
		return Pair{}, false
	}
}

func (s Syntax) PairOrDie() Pair {
	pair, ok := s.Pair()
	if !ok {
		panic("not a pair")
	}
	return pair
}

func (s Syntax) SourceLocationTree() *SourceLocationTree {
	wrapped, ok := s.datum.(WrappedSyntax)
	if !ok {
		return nil
	}
	return wrapped.SourceLocationTree()
}

func (s Syntax) SourceLocation() SourceLocation {
	if s.SourceLocationTree() == nil {
		return SourceLocation{}
	}
	return s.SourceLocationTree().SourceLocation
}

func (s Syntax) PrettyPrint(indent int) string {
	switch d := s.datum.(type) {
	case WrappedSyntax:
		return PrettyPrint(d.Datum(), indent)
	case Pair:
		return fmt.Sprintf("(%v%v", Syntax{d.First}.PrettyPrint(indent), Syntax{d.Rest}.prettyPrintRest(indent+1))
	default:
		panic(fmt.Sprintf("not syntax: %v", Write(d)))
	}
}

func (s Syntax) prettyPrintRest(indent int) string {
	indentString := strings.Repeat(" ", indent)
	switch d := s.datum.(type) {
	case WrappedSyntax:
		if d.Datum() == Null {
			return ")"
		}
		return fmt.Sprintf("\n%v.\n%v%v)", indentString, indentString, Syntax{s.datum}.PrettyPrint(indent))
	case Pair:
		return fmt.Sprintf("\n%v%v%v", indentString, Syntax{d.First}.PrettyPrint(indent), Syntax{d.Rest}.prettyPrintRest(indent))
	default:
		panic(fmt.Sprintf("not syntax: %v", Write(d)))
	}
}
