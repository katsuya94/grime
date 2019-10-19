package common

import (
	"fmt"
)

type EnvironmentProvider interface {
	Environment(phase int) Environment
}

// TODO: make map private
type Environment map[*Binding]Role

func NewEnvironment() Environment {
	return Environment{}
}

func (env *Environment) Extend(binding *Binding, role Role) {
	extended := make(Environment, len(*env))
	for b, r := range *env {
		extended[b] = r
	}
	extended[binding] = role
	*env = extended
}

func (env Environment) Lookup(binding *Binding) Role {
	role, _ := env[binding]
	return role
}

type Role interface {
	Export(*CpsTransformContext, Identifier) (Export, error)
}

type Variable struct{}

func NewVariable() Variable {
	return Variable{}
}

func (r Variable) Export(ctx *CpsTransformContext, id Identifier) (Export, error) {
	index := ctx.Add(id)
	return VariableTop{index}, nil
}

type SyntacticAbstraction struct {
	Transformer Procedure
}

func NewSyntacticAbstraction(transformer Procedure) SyntacticAbstraction {
	return SyntacticAbstraction{transformer}
}

func (r SyntacticAbstraction) Export(ctx *CpsTransformContext, id Identifier) (Export, error) {
	return SyntacticAbstractionTop{r.Transformer}, nil
}

type PatternLiteral struct {
	Id Identifier
}

func NewPatternLiteral(id Identifier) PatternLiteral {
	return PatternLiteral{id}
}

func (r PatternLiteral) Export(ctx *CpsTransformContext, id Identifier) (Export, error) {
	return nil, fmt.Errorf("top level pattern literal")
}

type PatternVariable struct {
	nesting int
}

func NewPatternVariable(nesting int) PatternVariable {
	return PatternVariable{nesting}
}

func (r PatternVariable) Export(ctx *CpsTransformContext, id Identifier) (Export, error) {
	return nil, fmt.Errorf("top level pattern variable")
}
