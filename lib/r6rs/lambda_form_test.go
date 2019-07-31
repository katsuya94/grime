package r6rs_test

import (
	"testing"

	"github.com/katsuya94/grime/common"
	. "github.com/katsuya94/grime/lib/r6rs"
	"github.com/katsuya94/grime/test"
)

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
