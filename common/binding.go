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

func (b *Binding) Identifier() Identifier {
	return b.id
}

func (b *Binding) String() string {
	return fmt.Sprintf("%p", b)
}
