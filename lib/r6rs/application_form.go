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

func (f ApplicationForm) CpsTransform(ctx CpsTransformContext) (common.Expression, error) {
	proc, err := f.Procedure.CpsTransform(ctx)
	if err != nil {
		return nil, err
	}
	args := make([]common.Expression, len(f.Arguments))
	for i := range f.Arguments {
		arg, err := f.Arguments[i].CpsTransform(ctx)
		if err != nil {
			return nil, err
		}
		args[i] = arg
	}
	return NewApplication(proc, args...), nil
}
