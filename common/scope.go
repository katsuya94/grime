package common

type substitution struct {
	id      Identifier
	binding Binding
}

type Scope struct {
	substitutions []substitution
}

func NewScope() *Scope {
	return &Scope{[]substitution{}}
}

func (s *Scope) Add(id Identifier, binding Binding) {
	s.substitutions = append(s.substitutions, substitution{id, binding})
}

func (s *Scope) lookup(id Identifier) (Binding, bool) {
	for _, substitution := range s.substitutions {
		if substitution.id.Binds(id) {
			return substitution.binding, true
		}
	}
	return Binding(-1), false
}

type scopeList map[int][]*Scope

func (sl scopeList) push(scope *Scope, phase int) {
	sl[phase] = append(sl[phase], scope)
}

func (sl scopeList) lookup(id Identifier, phase int) (Binding, bool) {
	scopes := sl[phase]
	for i := len(scopes) - 1; i >= 0; i-- {
		binding, ok := scopes[i].lookup(id)
		if ok {
			return binding, true
		}
	}
	return Binding(-1), false
}
