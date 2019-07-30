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
	coreForm := ReferenceForm{Id: Introduce(test.Syntax("id")).IdentifierOrDie()}
	actual := coreForm.Unexpand()
	expected := Introduce(test.Syntax("(#%reference id)"))
	test.AssertSyntaxEqual(t, expected, actual)
}

func TestLambdaForm_Unexpand(t *testing.T) {
	coreForm := LambdaForm{
		Formals: []common.Identifier{Introduce(test.Syntax("id")).IdentifierOrDie()},
		Inner:   LiteralForm{Datum: common.Boolean(true)},
	}
	actual := coreForm.Unexpand()
	expected := Introduce(test.Syntax("(#%lambda (id) (#%literal #t))"))
	test.AssertSyntaxEqual(t, expected, actual)
}

func TestApplicationForm_Unexpand(t *testing.T) {
	coreForm := ApplicationForm{
		Procedure: ReferenceForm{Id: Introduce(test.Syntax("id")).IdentifierOrDie()},
		Arguments: []CoreForm{LiteralForm{common.Boolean(true)}},
	}
	actual := coreForm.Unexpand()
	expected := Introduce(test.Syntax("(#%application (#%reference id) (#%literal #t))"))
	test.AssertSyntaxEqual(t, expected, actual)
}
