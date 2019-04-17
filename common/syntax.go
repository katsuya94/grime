package common

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
	return Identifier{id.WrappedSyntax.Mark(m).(WrappedSyntax)}
}

func (id Identifier) Equal(other Identifier) bool {
	return id.Name() == other.Name() && id.marks.equal(other.marks)
}

func (id Identifier) Bind(location Location) Identifier {
	scope := NewScope()
	scope.Set(id, location)
	id, _ = id.Push(scope, LEXICAL).Identifier()
	return id
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
		panic("unhandled syntax")
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
		panic("unhandled syntax")
	}
}

func (s Syntax) Identifier() (Identifier, bool) {
	wrapped, ok := s.datum.(WrappedSyntax)
	if !ok {
		return Identifier{}, false
	}
	return wrapped.Identifier()
}

func (s Syntax) Unwrap() Datum {
	datum := s.datum
	if wrapped, ok := datum.(WrappedSyntax); ok {
		datum = wrapped.Datum()
	}
	return datum
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

func (s Syntax) SourceLocationTree() *SourceLocationTree {
	switch d := s.datum.(type) {
	case WrappedSyntax:
		return d.SourceLocationTree()
	default:
		return nil
	}
}

func (s Syntax) SourceLocation() SourceLocation {
	if s.SourceLocationTree() == nil {
		return SourceLocation{}
	}
	return s.SourceLocationTree().SourceLocation
}
