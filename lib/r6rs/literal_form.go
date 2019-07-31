package r6rs

import (
	"github.com/katsuya94/grime/common"
)

type LiteralForm struct {
	Datum common.Datum
}

func (f LiteralForm) Unexpand() common.Syntax {
	return common.NewSyntax(list(literalId.WrappedSyntax, introduce(f.Datum)))
}

func (f LiteralForm) CpsTransform(ctx CpsTransformContext) common.Expression {
	return Literal{f.Datum}
}
