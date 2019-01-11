package common

import (
	"fmt"
)

const LEXICAL = -1

type Scope interface {
	Get(Identifier) Location
	Set(Identifier, Location) error
}

type binding struct {
	marks    markSet
	location Location
}

type BaseScope map[Symbol][]binding

func NewScope() BaseScope {
	return make(BaseScope)
}

func (b BaseScope) Get(id Identifier) Location {
	bindings, _ := b[id.Name()]
	for _, binding := range bindings {
		if id.marks.subset(binding.marks) {
			return binding.location
		}
	}
	return nil
}

func (b BaseScope) Set(id Identifier, location Location) error {
	bindings, _ := b[id.Name()]
	for _, binding := range bindings {
		if id.marks.subset(binding.marks) {
			return fmt.Errorf("already defined: %v", id.Name())
		}
	}
	b[id.Name()] = append(bindings, binding{
		marks:    id.marks,
		location: location,
	})
	return nil
}

func (b BaseScope) Bindings() map[Symbol]Location {
	m := make(map[Symbol]Location)
	for name, bindings := range b {
		for _, binding := range bindings {
			if (markSet{}).subset(binding.marks) {
				m[name] = binding.location
			}
		}
	}
	return m
}

type ProxyScope struct {
	BaseScope
	Scope
}

func NewProxyScope(s Scope) ProxyScope {
	return ProxyScope{NewScope(), s}
}

func (p ProxyScope) Get(id Identifier) Location {
	return p.BaseScope.Get(id)
}

func (p ProxyScope) Set(id Identifier, location Location) error {
	err := p.Scope.Set(id, location)
	if err != nil {
		return err
	}
	return p.BaseScope.Set(id, location)
}

type scopeList struct {
	Scope
	*scopeList
	phase int
}

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
	for l := id.scopeList; l != nil; l = l.scopeList {
		if l.phase != LEXICAL && l.phase != id.phase {
			continue
		}
		location := l.Get(id)
		if location != nil {
			return location
		}
	}
	return nil
}

func (id Identifier) Mark(m *M) Identifier {
	return Identifier{id.WrappedSyntax.Mark(m).(WrappedSyntax)}
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

// TODO: Rename to Unwrap
func (s Syntax) Datum() Datum {
	datum := s.datum
	if wrapped, ok := datum.(WrappedSyntax); ok {
		datum = wrapped.Datum()
	}
	return datum
}

// TODO: Rename to Datum
func (s Syntax) Form() Datum {
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
