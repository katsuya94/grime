package r6rs

import (
	"fmt"

	"github.com/katsuya94/grime/common"
)

type ReferenceForm struct {
	Id common.Identifier
}

func (f ReferenceForm) Unexpand() common.Syntax {
	return common.NewSyntax(list(referenceId.WrappedSyntax, f.Id.WrappedSyntax))
}

func (f ReferenceForm) CpsTransform(ctx CpsTransformContext) (common.Expression, error) {
	binding, ok := f.Id.Binding()
	if !ok {
		return nil, fmt.Errorf("cps transform: unbound identifier %v at %v", f.Id.Name(), f.Id.SourceLocation())
	}
	index := ctx.Index(binding)
	return NewReference(index, f.Id), nil
}
