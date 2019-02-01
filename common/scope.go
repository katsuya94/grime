package common

import "fmt"

const LEXICAL = -1

type scopeListElement struct {
	Scope
	phase int
}

type ScopeList []scopeListElement

func (l *ScopeList) Get(id Identifier) Location {
	for i := len(*l) - 1; i >= 0; i-- {
		element := (*l)[i]
		if element.phase != LEXICAL && element.phase != id.phase {
			continue
		}
		location := element.Get(id)
		if location != nil {
			return location
		}
	}
	return nil
}

func (l *ScopeList) Push(scope Scope, phase int) {
	*l = append(*l, scopeListElement{scope, phase})
}

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
	l = f.Scope.Get(id)
	if l != nil {
		return fmt.Errorf("already defined: %v", id.Name())
	}
	return f.BaseScope.Set(id, location)
}

func (p FlushScope) Flush() {
	for name, location := range p.BaseScope.Bindings() {
		p.Scope.Set(NewIdentifier(name), location)
	}
}
