package r6rs_test

import (
	"testing"

	"github.com/katsuya94/grime/common"
	. "github.com/katsuya94/grime/lib/r6rs"
	"github.com/katsuya94/grime/test"
)

func TestIfForm_Unexpand(t *testing.T) {
	condition := QuoteForm{Datum: common.Boolean(true), Mark: common.NewMark()}
	then := QuoteForm{Datum: common.Symbol("id"), Mark: common.NewMark()}
	otherwise := QuoteForm{Datum: common.Symbol("name"), Mark: common.NewMark()}
	coreForm := IfForm{Condition: condition, Then: then, Otherwise: otherwise, Mark: common.NewMark()}
	actual := coreForm.Unexpand()
	expected := Introduce(test.Syntax("(if (quote #t) (quote id) (quote name))"))
	test.AssertSyntaxEqual(t, expected, actual)
}

func TestIfForm_CpsTransform(t *testing.T) {
	ctx := NewCpsTransformContext([]Global{})
	condition := test.NewVoidExpression()
	then := test.NewVoidExpression()
	otherwise := test.NewVoidExpression()
	coreForm := IfForm{
		Condition: spyForm{cpsTransform: func(ctx *CpsTransformContext) (common.Expression, error) {
			return condition, nil
		}},
		Then: spyForm{cpsTransform: func(ctx *CpsTransformContext) (common.Expression, error) {
			return then, nil
		}},
		Otherwise: spyForm{cpsTransform: func(ctx *CpsTransformContext) (common.Expression, error) {
			return otherwise, nil
		}},
		Mark: common.NewMark(),
	}
	expected := NewIf(condition, then, otherwise)
	testCpsTransform(t, ctx, coreForm, expected)
}
