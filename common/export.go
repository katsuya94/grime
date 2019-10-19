package common

type Export interface {
	Portable(EvaluationContext) Portable
}

type VariableTop struct {
	index int
}

func (t VariableTop) Portable(ctx EvaluationContext) Portable {
	return VariablePortable{ctx.Get(t.index)}
}

type SyntacticAbstractionTop struct {
	transformer Procedure
}

func (t SyntacticAbstractionTop) Portable(ctx EvaluationContext) Portable {
	return SyntacticAbstractionPortable{t.transformer}
}
