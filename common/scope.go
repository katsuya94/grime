package common

type substitution struct {
	id      Identifier
	binding *Binding
}

type Scope struct {
	substitutions []substitution
}

func NewScope() *Scope {
	return &Scope{[]substitution{}}
}

func (s Scope) Identifiers() []Identifier {
	ids := make([]Identifier, len(s.substitutions))
	for i, substitution := range s.substitutions {
		ids[i] = substitution.id
	}
	return ids
}

func (s *Scope) add(id Identifier, binding *Binding) {
	s.substitutions = append(s.substitutions, substitution{id, binding})
}

func (s *Scope) lookup(id Identifier) *Binding {
	for _, substitution := range s.substitutions {
		if substitution.id.Binds(id) {
			return substitution.binding
		}
	}
	return nil
}

type scopeList []*Scope

func (sl scopeList) push(scope *Scope) scopeList {
	return append(sl, scope)
}

func (sl scopeList) lookup(id Identifier) *Binding {
	for i := len(sl) - 1; i >= 0; i-- {
		binding := sl[i].lookup(id)
		if binding != nil {
			return binding
		}
	}
	return nil
}
