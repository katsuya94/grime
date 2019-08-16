package r6rs_test

import (
	"testing"

	"github.com/katsuya94/grime/common"
	. "github.com/katsuya94/grime/lib/r6rs"
	"github.com/katsuya94/grime/test"
)

func TestSequenceForm_Unexpand(t *testing.T) {
	coreForm := SequenceForm{
		Forms: []CoreForm{
			LiteralForm{Datum: common.Boolean(true), Mark: common.NewMark()},
			LiteralForm{Datum: common.Boolean(false), Mark: common.NewMark()},
		},
		Mark: common.NewMark(),
	}
	actual := coreForm.Unexpand()
	expected := Introduce(test.Syntax("(#%sequence (#%literal #t) (#%literal #f))"))
	test.AssertSyntaxEqual(t, expected, actual)
}

func TestSequenceForm_CpsTransform(t *testing.T) {
	id := test.Identifier("id")
	location := common.NewLocation()
	ctx := NewCpsTransformContext([]Global{{id, location}})
	ctx.Add(id)
	coreForm := SequenceForm{
		Forms: []CoreForm{
			LiteralForm{Datum: common.Boolean(true), Mark: common.NewMark()},
			LiteralForm{Datum: common.Boolean(false), Mark: common.NewMark()},
		},
		Mark: common.NewMark(),
	}
	expected := NewSequence([]common.Expression{
		NewLiteral(common.Boolean(true)),
		NewLiteral(common.Boolean(false)),
	})
	testCpsTransform(t, ctx, coreForm, expected)
}
