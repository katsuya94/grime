package r6rs

import (
	"fmt"

	"github.com/katsuya94/grime/common"
)

type Environment struct {
	roles map[Binding]Role
}

func NewEnvironment() Environment {
	return Environment{map[Binding]Role{}}
}

func (env *Environment) Extend(binding Binding, role Role) {
	extended := make(map[Binding]Role, len(env.roles))
	for b, r := range env.roles {
		extended[b] = r
	}
	extended[binding] = role
	env.roles = extended
}

func (env Environment) Lookup(binding Binding) Role {
	role, _ := env.roles[binding]
	return role
}

type Role interface {
	Description() string
}

type SyntacticAbstraction struct {
	transformer common.Procedure
}

func NewSyntacticAbstraction(transformer common.Procedure) SyntacticAbstraction {
	return SyntacticAbstraction{transformer}
}

func (r SyntacticAbstraction) Description() string {
	return fmt.Sprintf("%#v", r.transformer)
}
