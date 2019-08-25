package r6rs

import (
	"github.com/katsuya94/grime/common"
)

type QuoteForm struct {
	Datum common.Datum
	Mark  *common.M
}

func (f QuoteForm) Unexpand() common.Syntax {
	return common.NewSyntax(list(QuoteId.WrappedSyntax, introduce(f.Datum)))
}

func (f QuoteForm) CpsTransform(ctx *CpsTransformContext) (common.Expression, error) {
	return NewQuote(f.Datum), nil
}
