package common

type DefineSyntaxForm struct {
	Name Symbol
	Form Datum
}

type DefineForm struct {
	Name Symbol
	Form Datum
}

type IfForm struct {
	Condition Datum
	Then      Datum
	Else      Datum
}

type LetForm struct {
	Name Symbol
	Init Datum
	Body Datum
}

type LetSyntaxForm struct{}

type BeginForm struct {
	Forms []Datum
}

type ApplicationForm struct {
	Procedure Datum
	Arguments []Datum
}

type LambdaForm struct {
	Formals []Symbol
	Body    Datum
}

type SetForm struct {
	Name Symbol
	Form Datum
}

type ReferenceForm struct {
	Name Symbol
}

type QuoteForm struct {
	Datum Datum
}

type SyntaxForm struct {
	Datum Datum
}
