package r6rs

import (
	"github.com/katsuya94/grime/common"
)

type SequenceForm struct {
	Forms []common.CoreForm
	Mark  *common.M
}

func (f SequenceForm) Unexpand() common.Syntax {
	forms := make([]common.Datum, len(f.Forms))
	for i := range forms {
		forms[i] = f.Forms[i].(Unexpander).Unexpand().Datum()
	}
	return common.NewSyntax(common.Pair{SequenceId.WrappedSyntax, list(forms...)})
}

func (f SequenceForm) CpsTransform(ctx *common.CpsTransformContext) (common.Expression, error) {
	var err error
	expressions := make([]common.Expression, len(f.Forms))
	for i := range expressions {
		expressions[i], err = f.Forms[i].CpsTransform(ctx)
		if err != nil {
			return nil, err
		}
	}
	return NewSequence(expressions), nil
}
