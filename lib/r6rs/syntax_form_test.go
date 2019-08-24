package r6rs_test

import (
	"testing"

	"github.com/katsuya94/grime/common"
	. "github.com/katsuya94/grime/lib/r6rs"
	"github.com/katsuya94/grime/test"
)

func TestSyntaxForm_Unexpand(t *testing.T) {
	template := Introduce(test.Syntax("#t"))
	coreForm := SyntaxForm{Template: template, Mark: common.NewMark()}
	actual := coreForm.Unexpand()
	expected := Introduce(test.Syntax("(syntax #t)"))
	test.AssertSyntaxEqual(t, expected, actual)
}

func TestSyntaxForm_CpsTransform(t *testing.T) {
	ctx := NewCpsTransformContext([]Global{})
	template := Introduce(test.Syntax("#t"))
	coreForm := SyntaxForm{Template: template, Mark: common.NewMark()}
	expected := NewSyntax(template)
	testCpsTransform(t, ctx, coreForm, expected)
}
