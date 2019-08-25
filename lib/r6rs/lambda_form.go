package r6rs

import (
	"github.com/katsuya94/grime/common"
)

type LambdaForm struct {
	Formals []common.Identifier
	Inner   CoreForm
	Mark    *common.M
}

func (f LambdaForm) Unexpand() common.Syntax {
	formals := make([]common.Datum, len(f.Formals))
	for i := range f.Formals {
		formals[i] = f.Formals[i].WrappedSyntax
	}
	return common.NewSyntax(list(LambdaId.WrappedSyntax, list(formals...), f.Inner.(Unexpander).Unexpand().Datum()))
}

func (f LambdaForm) CpsTransform(ctx *CpsTransformContext) (common.Expression, error) {
	ctx = ctx.New()
	indexes := make([]int, len(f.Formals))
	for i, formal := range f.Formals {
		indexes[i] = ctx.Add(formal)
	}
	inner, err := f.Inner.CpsTransform(ctx)
	if err != nil {
		return nil, err
	}
	return NewLambda(inner, indexes, ctx.EvaluationContextTemplate()), nil
}
