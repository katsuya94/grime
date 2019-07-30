package r6rs_test

import (
	"testing"

	"github.com/katsuya94/grime/common"
	. "github.com/katsuya94/grime/lib/r6rs"
	"github.com/katsuya94/grime/test"
)

func TestLiteralForm_Unexpand(t *testing.T) {
	coreForm := LiteralForm{Datum: common.Boolean(true)}
	actual := coreForm.Unexpand()
	expected := Introduce(test.Syntax("(#%literal #t)"))
	test.AssertSyntaxEqual(t, expected, actual)
}

func TestReferenceForm_Unexpand(t *testing.T) {
	id := Introduce(test.Syntax("id")).IdentifierOrDie()
	coreForm := ReferenceForm{Id: id}
	actual := coreForm.Unexpand()
	expected := Introduce(test.Syntax("(#%reference id)"))
	test.AssertSyntaxEqual(t, expected, actual)
}

func TestLambdaForm_Unexpand(t *testing.T) {
	id := Introduce(test.Syntax("id")).IdentifierOrDie()
	coreForm := LambdaForm{
		Formals: []common.Identifier{id},
		Inner:   LiteralForm{Datum: common.Boolean(true)},
	}
	actual := coreForm.Unexpand()
	expected := Introduce(test.Syntax("(#%lambda (id) (#%literal #t))"))
	test.AssertSyntaxEqual(t, expected, actual)
}

func TestApplicationForm_Unexpand(t *testing.T) {
	id := Introduce(test.Syntax("id")).IdentifierOrDie()
	coreForm := ApplicationForm{
		Procedure: ReferenceForm{Id: id},
		Arguments: []CoreForm{LiteralForm{common.Boolean(true)}},
	}
	actual := coreForm.Unexpand()
	expected := Introduce(test.Syntax("(#%application (#%reference id) (#%literal #t))"))
	test.AssertSyntaxEqual(t, expected, actual)
}
