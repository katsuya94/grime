package r6rs

import "github.com/katsuya94/grime/common"

type substitution struct {
	id      common.Identifier
	binding Binding
}

type Scope struct {
	substitutions []substitution
}

func NewScope() *Scope {
	return &Scope{[]substitution{}}
}

func (s *Scope) Add(id common.Identifier, binding Binding) {
	s.substitutions = append(s.substitutions, substitution{id, binding})
}

type ScopeList struct {
	scopes map[int][]*Scope
}

func NewScopeList() ScopeList {
	return ScopeList{map[int][]*Scope{}}
}

func (sl ScopeList) Push(scope *Scope, phase int) {
	sl.scopes[phase] = append(sl.scopes[phase], scope)
}
