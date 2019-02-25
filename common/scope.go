package common

import "fmt"

const LEXICAL = -1

type scopeListElement struct {
	Scope
	phase int
}

// ScopeList is a stack of Scopes, giving the top (innermost) scope precendence when resolving bindings. Implemented as a linked list, ensuring that syntaxes referencing outer scopes are not affected.
type ScopeList struct {
	scope Scope
	phase int
	next  *ScopeList
}

func NewScopeList() *ScopeList {
	return nil
}

func (l *ScopeList) Get(id Identifier) Location {
	if l == nil {
		return nil
	}
	if l.phase == LEXICAL || l.phase == id.phase {
		location := l.scope.Get(id)
		if location != nil {
			return location
		}
	}
	return l.next.Get(id)
}

func (l *ScopeList) Push(scope Scope, phase int) *ScopeList {
	return &ScopeList{scope, phase, l}
}

// Scope is a set of mappings from Identifiers to Locations.
type Scope interface {
	Get(Identifier) Location
	Set(Identifier, Location) error
}

func NewScope() BaseScope {
	return BaseScope{}
}

type binding struct {
	marks    markSet
	location Location
}

// BaseScope is a simple implementation of Scope.
type BaseScope map[Symbol][]binding

func (b BaseScope) Get(id Identifier) Location {
	bindings, _ := b[id.Name()]
	for _, binding := range bindings {
		if id.CapturedBy(binding) {
			return binding.location
		}
	}
	return nil
}

func (b BaseScope) Set(id Identifier, location Location) error {
	bindings, _ := b[id.Name()]
	for _, binding := range bindings {
		if id.CapturedBy(binding) {
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
	m := map[Symbol]Location{}
	for name, bindings := range b {
		for _, binding := range bindings {
			if binding.Unmarked() {
				m[name] = binding.location
			}
		}
	}
	return m
}

// ProxyScope wraps another scope, setting bindings both on itself and the underlying scope. This allows bindings to be treated lexically (available at any phase in its lexical context), as well as globally (exported at a particular phase).
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

// FlushScope wraps another scope, buffering bindings, and allowing unmarked bindings to later be flushed to the underlying scope. This allows isolating compilation from a shared scope.
type FlushScope struct {
	BaseScope
	Scope
}

func NewFlushScope(s Scope) FlushScope {
	return FlushScope{NewScope(), s}
}

func (f FlushScope) Get(id Identifier) Location {
	location := f.BaseScope.Get(id)
	if location != nil {
		return location
	}
	return f.Scope.Get(id)
}

func (f FlushScope) Set(id Identifier, location Location) error {
	l := f.Scope.Get(id)
	if l != nil {
		return fmt.Errorf("already defined: %v", id.Name())
	}
	return f.BaseScope.Set(id, location)
}

func (p FlushScope) Flush() {
	for name, location := range p.BaseScope.Bindings() {
		err := p.Scope.Set(NewIdentifier(name), location)
		if err != nil {
			panic(fmt.Sprintf("encountered error while flushing scope: %v", err))
		}
	}
}
