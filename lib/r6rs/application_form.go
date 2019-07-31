package r6rs

import (
	"github.com/katsuya94/grime/common"
)

type ApplicationForm struct {
	Procedure CoreForm
	Arguments []CoreForm
}

func (f ApplicationForm) Unexpand() common.Syntax {
	proc := f.Procedure.Unexpand().Datum()
	args := make([]common.Datum, len(f.Arguments))
	for i := range f.Arguments {
		args[i] = f.Arguments[i].Unexpand().Datum()
	}
	return common.NewSyntax(common.Pair{applicationId.WrappedSyntax, common.Pair{proc, list(args...)}})
}

func (f ApplicationForm) CpsTransform(ctx CpsTransformContext) common.Expression {
	return nil
}
