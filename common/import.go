package common

type Import interface {
	Provide(ctx EvaluationContext)
}

type VariableImport struct {
	index    int
	location Location
}

func (i VariableImport) Provide(ctx EvaluationContext) {
	ctx.locations[i.index] = i.location
}

type SyntacticAbstractionImport struct{}

func (i SyntacticAbstractionImport) Provide(ctx EvaluationContext) {}
