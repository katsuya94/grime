package core

import "github.com/katsuya94/grime/common"

type DefineSyntaxForm struct {
	Identifier common.Identifier
	Form       common.Datum
}

type SyntaxCaseForm struct {
	Input    common.Datum
	Literals []common.Identifier
	Patterns []common.Datum
	Fenders  []common.Datum
	Outputs  []common.Datum
}

type DefineForm struct {
	Identifier common.Identifier
	Form       common.Datum
}

type IfForm struct {
	Condition common.Datum
	Then      common.Datum
	Else      common.Datum
}

type LetForm struct {
	Identifier common.Identifier
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
	Formals []common.Identifier
	Body    []common.Datum
}

type SetForm struct {
	Identifier common.Identifier
	Form       common.Datum
}

type ReferenceForm struct {
	Identifier common.Identifier
}

type QuoteForm struct {
	Datum common.Datum
}

type SyntaxForm struct {
	Datum common.Datum
}
