package r6rs_test

import (
	"testing"

	. "github.com/katsuya94/grime/lib/r6rs"
	"github.com/katsuya94/grime/test"
)

func TestReferenceForm_Unexpand(t *testing.T) {
	id := Introduce(test.Syntax("id")).IdentifierOrDie()
	coreForm := ReferenceForm{Id: id}
	actual := coreForm.Unexpand()
	expected := Introduce(test.Syntax("(#%reference id)"))
	test.AssertSyntaxEqual(t, expected, actual)
}

func TestReferenceForm_CpsTransform(t *testing.T) {
	ctx := NewCpsTransformContext()
	id := test.Identifier("id")
	ctx.Add(id)
	coreForm := ReferenceForm{Id: id}
	expected := NewReference(ctx.IndexOrDie(id), id)
	testCpsTransform(t, ctx, coreForm, expected)
}
