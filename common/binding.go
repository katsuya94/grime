package common

import (
	"fmt"
)

type Binding struct {
	id Identifier
}

func newBinding(id Identifier) *Binding {
	return &Binding{id}
}

func Bind(id Identifier, scope *Scope, phase int) (Identifier, *Binding) {
	id = id.Push(scope, phase).IdentifierOrDie()
	binding := newBinding(id)
	scope.add(id, binding)
	return id, binding
}

func Rebind(id Identifier, binding *Binding, scope *Scope, phase int) Identifier {
	id = id.Push(scope, phase).IdentifierOrDie()
	scope.add(id, binding)
	return id
}

func (b *Binding) Identifier() Identifier {
	return b.id
}

func (b *Binding) String() string {
	return fmt.Sprintf("%p", b)
}
