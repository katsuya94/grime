package r6rs_test

import (
	"testing"

	"github.com/katsuya94/grime/common"
	. "github.com/katsuya94/grime/lib/r6rs"
	"github.com/katsuya94/grime/test"
)

func TestSequenceForm_Unexpand(t *testing.T) {
	coreForm := SequenceForm{
		Forms: []common.CoreForm{
			QuoteForm{Datum: common.Boolean(true), Mark: common.NewMark()},
			QuoteForm{Datum: common.Boolean(false), Mark: common.NewMark()},
		},
		Mark: common.NewMark(),
	}
	actual := coreForm.Unexpand()
	expected := Introduce(test.Syntax("(#%sequence (quote #t) (quote #f))"))
	test.AssertSyntaxEqual(t, expected, actual)
}

func TestSequenceForm_CpsTransform(t *testing.T) {
	id := test.Identifier("id")
	location := common.NewLocation()
	ctx := common.NewCpsTransformContext([]common.Global{{id, location}})
	ctx.Add(id)
	coreForm := SequenceForm{
		Forms: []common.CoreForm{
			QuoteForm{Datum: common.Boolean(true), Mark: common.NewMark()},
			QuoteForm{Datum: common.Boolean(false), Mark: common.NewMark()},
		},
		Mark: common.NewMark(),
	}
	expected := NewSequence([]common.Expression{
		NewQuote(common.Boolean(true)),
		NewQuote(common.Boolean(false)),
	})
	testCpsTransform(t, ctx, coreForm, expected)
}
