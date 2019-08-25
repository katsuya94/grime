package r6rs

import (
	"github.com/katsuya94/grime/common"
)

type TopForm struct {
	Id   common.Identifier
	Mark *common.M
}

func (f TopForm) Unexpand() common.Syntax {
	return common.NewSyntax(list(TopId.WrappedSyntax, f.Id.WrappedSyntax))
}

func (f TopForm) CpsTransform(ctx *CpsTransformContext) (common.Expression, error) {
	location, err := ctx.Top(f.Id)
	if err != nil {
		return nil, err
	}
	return NewTop(location), nil
}
