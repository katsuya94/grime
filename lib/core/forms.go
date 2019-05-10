package core

import "github.com/katsuya94/grime/common"

func markIds(in []common.Identifier, m *common.M) []common.Identifier {
	out := make([]common.Identifier, len(in))
	for i := range in {
		out[i] = in[i].Mark(m)
	}
	return out
}

func markMany(in []common.Syntax, m *common.M) []common.Syntax {
	out := make([]common.Syntax, len(in))
	for i := range in {
		out[i] = in[i].Mark(m)
	}
	return out
}

type DefineSyntaxForm struct {
	Identifier common.Identifier
	Form       common.Syntax
}

func (f DefineSyntaxForm) Mark(m *common.M) common.Marker {
	return DefineSyntaxForm{
		f.Identifier.Mark(m),
		f.Form.Mark(m),
	}
}

type SyntaxCaseForm struct {
	Input    common.Syntax
	Literals []common.Identifier
	Patterns []common.Syntax
	Fenders  []common.Syntax
	Outputs  []common.Syntax
}

func (f SyntaxCaseForm) Mark(m *common.M) common.Marker {
	return SyntaxCaseForm{
		f.Input.Mark(m),
		markIds(f.Literals, m),
		markMany(f.Patterns, m),
		markMany(f.Fenders, m),
		markMany(f.Outputs, m),
	}
}

type DefineForm struct {
	Identifier common.Identifier
	Form       common.Syntax
}

func (f DefineForm) Mark(m *common.M) common.Marker {
	return DefineForm{
		f.Identifier.Mark(m),
		f.Form.Mark(m),
	}
}

type IfForm struct {
	Condition common.Syntax
	Then      common.Syntax
	Else      common.Syntax
}

func (f IfForm) Mark(m *common.M) common.Marker {
	return IfForm{
		f.Condition.Mark(m),
		f.Then.Mark(m),
		f.Else.Mark(m),
	}
}

type LetForm struct {
	Identifier common.Identifier
	Init       common.Syntax
	Body       []common.Syntax
}

func (f LetForm) Mark(m *common.M) common.Marker {
	return LetForm{
		f.Identifier.Mark(m),
		f.Init.Mark(m),
		markMany(f.Body, m),
	}
}

type LetSyntaxForm struct{}

func (f LetSyntaxForm) Mark(m *common.M) common.Marker {
	return LetSyntaxForm{}
}

type BeginForm struct {
	Forms []common.Syntax
}

func (f BeginForm) Mark(m *common.M) common.Marker {
	return BeginForm{
		markMany(f.Forms, m),
	}
}

type ApplicationForm struct {
	Procedure common.Syntax
	Arguments []common.Syntax
}

func (f ApplicationForm) Mark(m *common.M) common.Marker {
	return ApplicationForm{
		f.Procedure.Mark(m),
		markMany(f.Arguments, m),
	}
}

type LambdaForm struct {
	Formals []common.Identifier
	Body    []common.Syntax
}

func (f LambdaForm) Mark(m *common.M) common.Marker {
	return LambdaForm{
		markIds(f.Formals, m),
		markMany(f.Body, m),
	}
}

// TODO: Rename SetForm -> AssignmentForm
type SetForm struct {
	Identifier common.Identifier
	Form       common.Syntax
}

func (f SetForm) Mark(m *common.M) common.Marker {
	return SetForm{
		f.Identifier.Mark(m),
		f.Form.Mark(m),
	}
}

type ReferenceForm struct {
	Identifier common.Identifier
}

func (f ReferenceForm) Mark(m *common.M) common.Marker {
	return ReferenceForm{
		f.Identifier.Mark(m),
	}
}

type QuoteForm struct {
	Datum common.Datum
}

func (f QuoteForm) Mark(m *common.M) common.Marker {
	return f
}

type SyntaxForm struct {
	Template common.Syntax
}

func (f SyntaxForm) Mark(m *common.M) common.Marker {
	return SyntaxForm{
		f.Template.Mark(m),
	}
}
