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

func TestLiteralForm_CpsTransform(t *testing.T) {
	ctx := NewCpsTransformContext([]Global{})
	coreForm := LiteralForm{Datum: common.Boolean(true)}
	expected := NewLiteral(common.Boolean(true))
	testCpsTransform(t, ctx, coreForm, expected)
}
