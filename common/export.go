package common

type Export interface {
	Portable(EvaluationContext) Portable
}

type VariableExport struct {
	index int
}

func (t VariableExport) Portable(ctx EvaluationContext) Portable {
	return VariablePortable{ctx.Get(t.index)}
}

type SyntacticAbstractionExport struct {
	transformer Procedure
}

func (t SyntacticAbstractionExport) Portable(ctx EvaluationContext) Portable {
	return SyntacticAbstractionPortable{t.transformer}
}
