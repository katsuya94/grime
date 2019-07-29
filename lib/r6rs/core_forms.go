package r6rs

import (
	"github.com/katsuya94/grime/common"
)

type EqualityContext interface{}

type CoreForm interface {
	Unexpand() common.Syntax
}

type LiteralForm struct {
	Datum common.Datum
}

func (f LiteralForm) Unexpand() common.Syntax {
	return common.NewSyntax(list(literalId, f.Datum))
}

type ReferenceForm struct {
	Id common.Identifier
}

func (f ReferenceForm) Unexpand() common.Syntax {
	return common.NewSyntax(list(common.Symbol("#%reference"), f.Id.WrappedSyntax))
}

type LambdaForm struct {
	Formals []common.Identifier
	Inner   CoreForm
}

func (f LambdaForm) Unexpand() common.Syntax {
	formals := make([]common.Datum, len(f.Formals))
	for i := range f.Formals {
		formals[i] = f.Formals[i].Datum()
	}
	return common.NewSyntax(list(common.Symbol("#%lambda"), list(formals...), f.Inner.Unexpand()))
}

type ApplicationForm struct {
	Procedure CoreForm
	Arguments []CoreForm
}

func (f ApplicationForm) Unexpand() common.Syntax {
	args := make([]common.Datum, len(f.Arguments))
	for i := range f.Arguments {
		args[i] = f.Arguments[i].Unexpand()
	}
	return common.NewSyntax(common.Pair{f.Procedure.Unexpand(), list(args...)})
}

func list(data ...common.Datum) common.Datum {
	if len(data) == 0 {
		return common.Null
	} else {
		return common.Pair{data[0], list(data[1:]...)}
	}
}
