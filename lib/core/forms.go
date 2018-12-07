package core

import "github.com/katsuya94/grime/common"

type DefineSyntaxForm struct {
	Identifier common.WrappedSyntax
	Form       common.Datum
}

type SyntaxCaseForm struct {
	Input    common.Datum
	Literals []common.WrappedSyntax
	Patterns []common.Datum
	Fenders  []common.Datum
	Outputs  []common.Datum
}

type DefineForm struct {
	Identifier common.WrappedSyntax
	Form       common.Datum
}

type IfForm struct {
	Condition common.Datum
	Then      common.Datum
	Else      common.Datum
}

type LetForm struct {
	Identifier common.WrappedSyntax
	Init       common.Datum
	Body       []common.Datum
}

type LetSyntaxForm struct{}

type BeginForm struct {
	Forms []common.Datum
}

type ApplicationForm struct {
	Procedure common.Datum
	Arguments []common.Datum
}

type LambdaForm struct {
	Formals []common.WrappedSyntax
	Body    []common.Datum
}

type SetForm struct {
	Identifier common.WrappedSyntax
	Form       common.Datum
}

type ReferenceForm struct {
	Identifier common.WrappedSyntax
}

type QuoteForm struct {
	Datum common.Datum
}

type SyntaxForm struct {
	Datum common.Datum
}
