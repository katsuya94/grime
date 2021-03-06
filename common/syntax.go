package common

import "fmt"

// Identifier wraps a wrapped symbol (an identifier), providing access to its binding.
type Identifier struct {
	WrappedSyntax
}

func NewIdentifier(name Symbol) Identifier {
	return Identifier{NewWrappedSyntax(name, nil)}
}

func (id Identifier) Name() Symbol {
	return id.datum.(Symbol)
}

func (id Identifier) Location() Location {
	return id.scopeList.Get(id)
}

func (id Identifier) Mark(m *M) Identifier {
	return Identifier{NewSyntax(id.WrappedSyntax).Mark(m).Datum().(WrappedSyntax)}
}

func (id Identifier) CapturedBy(other Identifier) bool {
	return id.Name() == other.Name() && id.marks.contains(other.marks)
}

func (id Identifier) BoundEqual(other Identifier) bool {
	return id.Name() == other.Name() && (id.marks.contains(other.marks) || other.marks.contains(id.marks))
}

func (id Identifier) Equal(other Identifier) bool {
	return id.Name() == other.Name() && id.marks.equal(other.marks)
}

func (id Identifier) FreeEqual(other Identifier) bool {
	idLocation := id.Location()
	otherLocation := other.Location()
	if idLocation == nil && otherLocation == nil {
		return id.Name() == other.Name()
	} else if idLocation != nil && otherLocation != nil {
		return idLocation == otherLocation
	} else {
		return false
	}
}

func (id Identifier) Bind(location Location) Identifier {
	scope := NewScope()
	scope.Set(id, location)
	id, ok := id.Push(scope, LEXICAL).Identifier()
	if !ok {
		panic("expected identifier")
	}
	return id
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

func (s Syntax) Push(scope Scope, phase int) Syntax {
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
