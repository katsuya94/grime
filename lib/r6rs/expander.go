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

type Expander interface {
	Expand(common.Syntax, common.Environment) (CoreForm, error)
}
