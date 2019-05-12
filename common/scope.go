package common

import "fmt"

const LEXICAL = -1

// ScopeList is a stack of Scopes, giving the top (innermost) scope precendence when resolving bindings. Implemented as a linked list, ensuring that syntaxes referencing outer scopes are not affected.
type ScopeList struct {
	scope Scope
	phase int
	frame bool
	next  *ScopeList
}

func NewScopeList() *ScopeList {
	return nil
}

func (sl *ScopeList) Get(id Identifier) BindingStackContext {
	return sl.get(id, CurrentStackContext)
}

func (sl *ScopeList) get(id Identifier, stackContext StackContext) BindingStackContext {
	if sl == nil {
		return NilBindingStackContext
	}
	if sl.phase == LEXICAL || sl.phase == id.phase {
		binding := sl.scope.Get(id)
		if binding != nil {
			return BindingStackContext{binding, stackContext}
		}
	}
	if sl.frame {
		stackContext++
	}
	return sl.next.get(id, stackContext)
}

func (sl *ScopeList) Push(scope Scope, phase int, frame bool) *ScopeList {
	return &ScopeList{scope, phase, frame, sl}
}

// Scope is a set of mappings from Identifiers to Bindings.
type Scope interface {
	Get(Identifier) Binding
	Set(Identifier, Binding) error
}

// https://docs.racket-lang.org/reference/syntax-model.html
// For a given identifier, multiple bindings may have scope sets that are subsets of the identifier’s; in that case, the identifier refers to the binding whose set is a superset of all others; if no such binding exists, the reference is ambiguous (and triggers a syntax error if it is parsed as an expression).

func NewScope() BaseScope {
	return BaseScope{}
}

type scopeEntry struct {
	marks   markSet
	binding Binding
}

// BaseScope is a simple implementation of Scope.
type BaseScope map[Symbol][]scopeEntry

func (b BaseScope) Get(id Identifier) Binding {
	entries, _ := b[id.Name()]
	markSets := make([]markSet, len(entries))
	for i, entry := range entries {
		markSets[i] = entry.marks
	}
	i, ok := indexSuperSetContainedBy(id.marks, markSets...)
	if !ok {
		panic(fmt.Sprintf("ambiguous binding: %v", id.Name()))
	} else if i == -1 {
		return nil
	}
	return entries[i].binding
}

func (b BaseScope) Set(id Identifier, binding Binding) error {
	entries, _ := b[id.Name()]
	markSets := make([]markSet, len(entries)+1)
	for i, entry := range entries {
		markSets[i] = entry.marks
	}
	markSets[len(entries)] = id.marks
	// TODO: consider precomputing the graph
	if duplicateMarkSets(markSets...) {
		return fmt.Errorf("already defined: %v", id.Name())
	}
	b[id.Name()] = append(entries, scopeEntry{
		marks:   id.marks,
		binding: binding,
	})
	return nil
}

func (b BaseScope) Bindings() map[Symbol]Binding {
	m := map[Symbol]Binding{}
	for name, bindings := range b {
		for _, binding := range bindings {
			if (markSet{}).contains(binding.marks) {
				m[name] = binding.binding
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

func (p ProxyScope) Get(id Identifier) Binding {
	return p.BaseScope.Get(id)
}

func (p ProxyScope) Set(id Identifier, binding Binding) error {
	err := p.Scope.Set(id, binding)
	if err != nil {
		return err
	}
	return p.BaseScope.Set(id, binding)
}

// FlushScope wraps another scope, buffering bindings, and allowing unmarked bindings to later be flushed to the underlying scope. This allows isolating compilation from a shared scope.
type FlushScope struct {
	BaseScope
	Scope
}

func NewFlushScope(s Scope) FlushScope {
	return FlushScope{NewScope(), s}
}

func (f FlushScope) Get(id Identifier) Binding {
	binding := f.BaseScope.Get(id)
	if binding != nil {
		return binding
	}
	return f.Scope.Get(id)
}

func (f FlushScope) Set(id Identifier, binding Binding) error {
	existing := f.Scope.Get(id)
	if existing != nil {
		return fmt.Errorf("already defined: %v", id.Name())
	}
	return f.BaseScope.Set(id, binding)
}

func (p FlushScope) Flush() {
	for name, binding := range p.BaseScope.Bindings() {
		err := p.Scope.Set(NewIdentifier(name), binding)
		if err != nil {
			panic(fmt.Sprintf("encountered error while flushing scope: %v", err))
		}
	}
}
