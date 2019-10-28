package common

type Portable interface {
	Import(ctx *CpsTransformContext, env Environment, id Identifier, binding *Binding) Import
}

type VariablePortable struct {
	location Location
}

func (p VariablePortable) Import(ctx *CpsTransformContext, env Environment, id Identifier, binding *Binding) Import {
	index := ctx.Add(id)
	env[binding] = NewVariable()
	return VariableImport{index, p.location}
}

type SyntacticAbstractionPortable struct {
	transformer Procedure
}

func NewSyntacticAbstractionPortable(transformer Procedure) SyntacticAbstractionPortable {
	return SyntacticAbstractionPortable{transformer}
}

func (p SyntacticAbstractionPortable) Import(ctx *CpsTransformContext, env Environment, id Identifier, binding *Binding) Import {
	env[binding] = NewSyntacticAbstraction(p.transformer)
	return SyntacticAbstractionImport{}
}
