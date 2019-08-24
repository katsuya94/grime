package r6rs

import (
	"github.com/katsuya94/grime/common"
)

type SyntaxForm struct {
	Template common.Syntax
	Mark     *common.M
}

func (f SyntaxForm) Unexpand() common.Syntax {
	return common.NewSyntax(list(SyntaxId.WrappedSyntax, f.Template.Datum()))
}

func (f SyntaxForm) CpsTransform(ctx CpsTransformContext) (common.Expression, error) {
	return NewSyntax(f.Template), nil
}
