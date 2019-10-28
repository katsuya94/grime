package common

// TODO: rename CoreForm (and file)

type CoreForm interface {
	CpsTransform(*CpsTransformContext) (Expression, error)
}

type ExpansionContext struct {
	Expander Expander
	Env      Environment
	Phase    int
}

type ExpanderFactory interface {
	Expander(EnvironmentProvider) Expander
}

type Expander interface {
	Expand(ExpansionContext, Syntax) (CoreForm, error)
	ExpandBody(ExpansionContext, []Syntax, *Scope) (CoreForm, Environment, error)
}
