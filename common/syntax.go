package common

import "fmt"

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
		if binding.marks.equal(id.marks) {
			return binding.location
		}
	}
	return nil
}

func (b BaseScope) Set(id Identifier, location Location) error {
	bindings, _ := b[id.Name()]
	for _, binding := range bindings {
		if binding.marks.equal(id.marks) {
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
			if binding.marks.equal(markSet{}) {
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
	Datum
}

func (s Syntax) Push(scope Scope, phase int) Syntax {
	switch d := s.Datum.(type) {
	case WrappedSyntax:
		return Syntax{d.Push(scope, phase)}
	case Pair:
		return Syntax{
			Pair{
				Syntax{d.First}.Push(scope, phase).Datum,
				Syntax{d.Rest}.Push(scope, phase).Datum,
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
	switch d := s.Datum.(type) {
	case WrappedSyntax:
		return Syntax{d.Next()}
	case Pair:
		return Syntax{
			Pair{
				Syntax{d.First}.Next().Datum,
				Syntax{d.Rest}.Next().Datum,
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
	wrapped, ok := s.Datum.(WrappedSyntax)
	if !ok {
		return Identifier{}, false
	}
	return wrapped.Identifier()
}
