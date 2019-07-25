package common

import "fmt"

type ScopeSet map[int]BaseScope

func NewScopeSet() ScopeSet {
	return ScopeSet{0: NewScope()}
}

func (ss ScopeSet) Set(phase int, name Symbol, binding Binding) error {
	_, ok := ss[phase]
	if !ok {
		ss[phase] = NewScope()
	}
	return ss[phase].Set(NewIdentifier(name), binding)
}

func (ss ScopeSet) Apply(syntax Syntax) Syntax {
	for phase, scope := range ss {
		syntax = syntax.Push(scope, phase, false)
	}
	return syntax
}

func (ss ScopeSet) Save(bindingsFrame BindingsFrame, frame *Frame, internal Symbol, external Symbol) error {
	exported := false
	for phase, scope := range ss {
		for name, binding := range scope.Bindings() {
			if internal != name {
				continue
			}
			bindingsFrame.Copy(external, phase, binding, frame)
			exported = true
		}
	}
	if !exported {
		return fmt.Errorf("can't export unbound identifier %v", internal)
	}
	return nil
}
