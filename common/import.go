package common

type Import interface {
	Provide(ctx EvaluationContext)
}

type VariableImport struct {
	index    int
	location Location
}

func (i VariableImport) Provide(ctx EvaluationContext) {
	// TODO
}

type SyntacticAbstractionImport struct{}

func (i SyntacticAbstractionImport) Provide(ctx EvaluationContext) {}
