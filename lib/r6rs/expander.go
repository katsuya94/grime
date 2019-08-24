package r6rs

import (
	"github.com/katsuya94/grime/common"
)

type ExpansionContext struct {
	Expander Expander
	Env      common.Environment
}

func (ctx ExpansionContext) Expand(syntax common.Syntax) (CoreForm, error) {
	return ctx.Expander.Expand(syntax, ctx.Env)
}

func (ctx ExpansionContext) Next() ExpansionContext {
	return ExpansionContext{ctx.Expander, ctx.Env.Next()}
}

type Expander interface {
	Expand(common.Syntax, common.Environment) (CoreForm, error)
}
