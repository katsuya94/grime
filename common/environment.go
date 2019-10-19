package common

import (
	"fmt"
)

type EnvironmentProvider interface {
	Environment(phase int) Environment
}

type Environment struct {
	roles map[*Binding]Role
}

func NewEnvironment() Environment {
	return Environment{map[*Binding]Role{}}
}

func (env *Environment) Extend(binding *Binding, role Role) {
	extended := make(map[*Binding]Role, len(env.roles))
	for b, r := range env.roles {
		extended[b] = r
	}
	extended[binding] = role
	env.roles = extended
}

func (env Environment) Lookup(binding *Binding) Role {
	role, _ := env.roles[binding]
	return role
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

type PatternVariable struct {
	nesting int
}

func NewPatternVariable(nesting int) PatternVariable {
	return PatternVariable{nesting}
}

func (r PatternVariable) Description() string {
	return fmt.Sprintf("PatternVariable{%#v}", r.nesting)
}
