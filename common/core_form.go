package common

// TODO: rename CoreForm (and file)

type CoreForm interface {
	CpsTransform(*CpsTransformContext) (Expression, error)
}

type ExpansionContext struct {
	Expander Expander
	Env      Environment
}

func (ctx ExpansionContext) Expand(syntax Syntax) (CoreForm, error) {
	return ctx.Expander.Expand(syntax, ctx.Env)
}

type Expander interface {
	Expand(Syntax, Environment) (CoreForm, error)
}
