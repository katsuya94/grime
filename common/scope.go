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

type scopeList map[int][]*Scope

func (sl scopeList) push(scope *Scope, phase int) scopeList {
	pushed := make(scopeList, len(sl))
	for phase, scopes := range sl {
		pushed[phase] = scopes
	}
	pushed[phase] = append(sl[phase], scope)
	return pushed
}

func (sl scopeList) lookup(id Identifier, phase int) *Binding {
	scopes := sl[phase]
	for i := len(scopes) - 1; i >= 0; i-- {
		binding := scopes[i].lookup(id)
		if binding != nil {
			return binding
		}
	}
	return nil
}
