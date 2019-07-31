package r6rs

import (
	"github.com/katsuya94/grime/common"
)

type LambdaForm struct {
	Formals []common.Identifier
	Inner   CoreForm
}

func (f LambdaForm) Unexpand() common.Syntax {
	formals := make([]common.Datum, len(f.Formals))
	for i := range f.Formals {
		formals[i] = f.Formals[i].WrappedSyntax
	}
	return common.NewSyntax(list(lambdaId.WrappedSyntax, list(formals...), f.Inner.Unexpand().Datum()))
}

func (f LambdaForm) CpsTransform(ctx CpsTransformContext) common.Expression {
	return nil
}
