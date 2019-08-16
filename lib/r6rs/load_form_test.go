package r6rs_test

import (
	"testing"

	"github.com/katsuya94/grime/common"
	. "github.com/katsuya94/grime/lib/r6rs"
	"github.com/katsuya94/grime/test"
)

func TestLoadForm_Unexpand(t *testing.T) {
	id := Introduce(test.Syntax("id")).IdentifierOrDie()
	coreForm := LoadForm{Id: id, Mark: common.NewMark()}
	actual := coreForm.Unexpand()
	expected := Introduce(test.Syntax("(#%load id)"))
	test.AssertSyntaxEqual(t, expected, actual)
}

func TestLoadForm_CpsTransform(t *testing.T) {
	ctx := NewCpsTransformContext([]Global{})
	id := test.Identifier("id")
	ctx.Add(id)
	coreForm := LoadForm{Id: id, Mark: common.NewMark()}
	expected := NewLoad(ctx.IndexOrDie(id), id)
	testCpsTransform(t, ctx, coreForm, expected)
}

func TestLoadForm_CpsTransformOutOfContext(t *testing.T) {
	ctx := NewCpsTransformContext([]Global{})
	id := test.Identifier("id")
	coreForm := LoadForm{Id: id, Mark: common.NewMark()}
	testCpsTransformError(t, ctx, coreForm, "cps transform: id not in context")
}
