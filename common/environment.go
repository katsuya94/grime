package common

import (
	"fmt"
)

type Environment struct {
	roles    map[int]map[*Binding]Role
	defaults map[*Binding]Role
}

func NewEnvironment(defaults map[*Binding]Role) Environment {
	return Environment{map[int]map[*Binding]Role{}, defaults}
}

func (env *Environment) Extend(binding *Binding, phase int, role Role) {
	extended := make(map[int]map[*Binding]Role, len(env.roles))
	for p, rs := range env.roles {
		extended[p] = make(map[*Binding]Role, len(rs))
		for b, r := range rs {
			extended[p][b] = r
		}
	}
	if _, ok := extended[phase]; !ok {
		extended[phase] = make(map[*Binding]Role, 1)
	}
	extended[phase][binding] = role
	env.roles = extended
}

func (env Environment) Lookup(binding *Binding, phase int) Role {
	role, _ := env.roles[phase][binding]
	if role != nil {
		return role
	}
	return env.defaults[binding]
}

type Role interface {
	Description() string
}

type Variable struct{}

func NewVariable() Variable {
	return Variable{}
}

func (r Variable) Description() string {
	return "Variable{}"
}

type SyntacticAbstraction struct {
	Transformer Procedure
}

func NewSyntacticAbstraction(transformer Procedure) SyntacticAbstraction {
	return SyntacticAbstraction{transformer}
}

func (r SyntacticAbstraction) Description() string {
	return fmt.Sprintf("SyntacticAbstraction{%#v}", r.Transformer)
}

type PatternLiteral struct {
	Id Identifier
}

func NewPatternLiteral(id Identifier) PatternLiteral {
	return PatternLiteral{id}
}

func (r PatternLiteral) Description() string {
	return fmt.Sprintf("PatternLiteral{%#v}", r.Id)
}
