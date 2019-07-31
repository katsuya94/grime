package r6rs

import (
	"github.com/katsuya94/grime/common"
)

type EqualityContext interface{}

type CoreForm interface {
	Unexpand() common.Syntax
	CpsTransform() common.Expression
}

type LiteralForm struct {
	Datum common.Datum
}

func (f LiteralForm) Unexpand() common.Syntax {
	return common.NewSyntax(list(literalId.WrappedSyntax, introduce(f.Datum)))
}

func (f LiteralForm) CpsTransform() common.Expression {
	return Literal{f.Datum}
}

type ReferenceForm struct {
	Id common.Identifier
}

func (f ReferenceForm) Unexpand() common.Syntax {
	return common.NewSyntax(list(referenceId.WrappedSyntax, f.Id.WrappedSyntax))
}

func (f ReferenceForm) CpsTransform() common.Expression {
	return nil
}

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

func (f LambdaForm) CpsTransform() common.Expression {
	return nil
}

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

func introduce(datum common.Datum) common.Datum {
	return Introduce(common.NewSyntax(common.NewWrappedSyntax(datum, nil))).Datum()
}

func list(data ...common.Datum) common.Datum {
	if len(data) == 0 {
		return introduce(common.Null)
	} else {
		return common.Pair{data[0], list(data[1:]...)}
	}
}

func (f ApplicationForm) CpsTransform() common.Expression {
	return nil
}
