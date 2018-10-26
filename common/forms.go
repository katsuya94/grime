package common

type Form interface{}

type DefineSyntaxForm struct {
	Name Symbol
	Form Form
}

type DefineForm struct {
	Name Symbol
	Form Form
}

type IfForm struct {
	Condition Form
	Then      Form
	Else      Form
}

type LetForm struct {
	Name Symbol
	Init Form
	Body Form
}

type LetSyntaxForm struct{}

type BeginForm struct {
	Forms []Form
}

type ApplicationForm struct {
	Procedure Form
	Arguments []Form
}

type LambdaForm struct {
	Formals []Symbol
	Body    Form
}

type SetForm struct {
	Name Symbol
	Form Form
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
