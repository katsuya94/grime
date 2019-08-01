package r6rs

import (
	"github.com/katsuya94/grime/common"
)

type ReferenceForm struct {
	Id common.Identifier
}

func (f ReferenceForm) Unexpand() common.Syntax {
	return common.NewSyntax(list(referenceId.WrappedSyntax, f.Id.WrappedSyntax))
}

func (f ReferenceForm) CpsTransform(ctx CpsTransformContext) (common.Expression, error) {
	index, err := ctx.Index(f.Id)
	if err != nil {
		return nil, err
	}
	return NewReference(index, f.Id), nil
}
