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

func (f ReferenceForm) CpsTransform(ctx CpsTransformContext) common.Expression {
	return nil
}
