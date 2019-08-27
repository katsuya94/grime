package r6rs_test

import (
	"testing"

	"github.com/katsuya94/grime/common"
	. "github.com/katsuya94/grime/lib/r6rs"
	"github.com/katsuya94/grime/test"
)

func TestQuoteForm_Unexpand(t *testing.T) {
	coreForm := QuoteForm{Datum: common.Boolean(true), Mark: common.NewMark()}
	actual := coreForm.Unexpand()
	expected := Introduce(test.Syntax("(quote #t)"))
	test.AssertSyntaxEqual(t, expected, actual)
}

func TestQuoteForm_CpsTransform(t *testing.T) {
	ctx := common.NewCpsTransformContext([]common.Global{})
	coreForm := QuoteForm{Datum: common.Boolean(true), Mark: common.NewMark()}
	expected := NewQuote(common.Boolean(true))
	testCpsTransform(t, ctx, coreForm, expected)
}
