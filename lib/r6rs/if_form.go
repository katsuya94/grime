package r6rs

import (
	"github.com/katsuya94/grime/common"
)

type IfForm struct {
	Condition common.CoreForm
	Then      common.CoreForm
	Otherwise common.CoreForm
	Mark      *common.M
}

func (f IfForm) Unexpand() common.Syntax {
	return common.NewSyntax(list(IfId.WrappedSyntax, f.Condition.(Unexpander).Unexpand().Datum(), f.Then.(Unexpander).Unexpand().Datum(), f.Otherwise.(Unexpander).Unexpand().Datum()))
}

func (f IfForm) CpsTransform(ctx *common.CpsTransformContext) (common.Expression, error) {
	condition, err := f.Condition.CpsTransform(ctx)
	if err != nil {
		return nil, err
	}
	then, err := f.Then.CpsTransform(ctx)
	if err != nil {
		return nil, err
	}
	otherwise, err := f.Otherwise.CpsTransform(ctx)
	if err != nil {
		return nil, err
	}
	return NewIf(condition, then, otherwise), nil
}
