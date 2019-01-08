package core

import "github.com/katsuya94/grime/common"

func markIds(in []common.Identifier, m *common.M) []common.Identifier {
	out := make([]common.Identifier, len(in))
	for i := range in {
		out[i] = in[i].Mark(m)
	}
	return out
}

func markData(in []common.Datum, m *common.M) []common.Datum {
	out := make([]common.Datum, len(in))
	for i := range in {
		out[i] = common.Mark(in[i], m)
	}
	return out
}

type DefineSyntaxForm struct {
	Identifier common.Identifier
	Form       common.Datum
}

func (f DefineSyntaxForm) Mark(m *common.M) common.Marker {
	return DefineSyntaxForm{
		f.Identifier.Mark(m),
		common.Mark(f.Form, m),
	}
}

type SyntaxCaseForm struct {
	Input    common.Datum
	Literals []common.Identifier
	Patterns []common.Datum
	Fenders  []common.Datum
	Outputs  []common.Datum
}

func (f SyntaxCaseForm) Mark(m *common.M) common.Marker {
	return SyntaxCaseForm{
		common.Mark(f.Input, m),
		markIds(f.Literals, m),
		markData(f.Patterns, m),
		markData(f.Fenders, m),
		markData(f.Outputs, m),
	}
}

type DefineForm struct {
	Identifier common.Identifier
	Form       common.Datum
}

func (f DefineForm) Mark(m *common.M) common.Marker {
	return DefineForm{
		f.Identifier.Mark(m),
		common.Mark(f.Form, m),
	}
}

type IfForm struct {
	Condition common.Datum
	Then      common.Datum
	Else      common.Datum
}

func (f IfForm) Mark(m *common.M) common.Marker {
	return IfForm{
		common.Mark(f.Condition, m),
		common.Mark(f.Then, m),
		common.Mark(f.Else, m),
	}
}

type LetForm struct {
	Identifier common.Identifier
	Init       common.Datum
	Body       []common.Datum
}

func (f LetForm) Mark(m *common.M) common.Marker {
	return LetForm{
		f.Identifier.Mark(m),
		common.Mark(f.Init, m),
		markData(f.Body, m),
	}
}

type LetSyntaxForm struct{}

func (f LetSyntaxForm) Mark(m *common.M) common.Marker {
	return LetSyntaxForm{}
}

type BeginForm struct {
	Forms []common.Datum
}

func (f BeginForm) Mark(m *common.M) common.Marker {
	return BeginForm{
		markData(f.Forms, m),
	}
}

type ApplicationForm struct {
	Procedure common.Datum
	Arguments []common.Datum
}

func (f ApplicationForm) Mark(m *common.M) common.Marker {
	return ApplicationForm{
		common.Mark(f.Procedure, m),
		markData(f.Arguments, m),
	}
}

type LambdaForm struct {
	Formals []common.Identifier
	Body    []common.Datum
}

func (f LambdaForm) Mark(m *common.M) common.Marker {
	return LambdaForm{
		markIds(f.Formals, m),
		markData(f.Body, m),
	}
}

type SetForm struct {
	Identifier common.Identifier
	Form       common.Datum
}

func (f SetForm) Mark(m *common.M) common.Marker {
	return SetForm{
		f.Identifier.Mark(m),
		common.Mark(f.Form, m),
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
	Datum common.Datum
}

func (f SyntaxForm) Mark(m *common.M) common.Marker {
	return SyntaxForm{
		common.Mark(f.Datum, m),
	}
}
