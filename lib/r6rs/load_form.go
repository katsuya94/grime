package r6rs

import (
	"github.com/katsuya94/grime/common"
)

type LoadForm struct {
	Id   common.Identifier
	Mark *common.M
}

func (f LoadForm) Unexpand() common.Syntax {
	return common.NewSyntax(list(LoadId.WrappedSyntax, f.Id.WrappedSyntax))
}

func (f LoadForm) CpsTransform(ctx *common.CpsTransformContext) (common.Expression, error) {
	index, err := ctx.Index(f.Id)
	if err != nil {
		return nil, err
	}
	return NewLoad(index, f.Id), nil
}
