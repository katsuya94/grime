package r6rs_test

import (
	"testing"

	"github.com/katsuya94/grime/common"
	. "github.com/katsuya94/grime/lib/r6rs"
	"github.com/katsuya94/grime/test"
)

func TestTopForm_Unexpand(t *testing.T) {
	id := Introduce(test.Syntax("id")).IdentifierOrDie()
	coreForm := TopForm{Id: id, Mark: common.NewMark()}
	actual := coreForm.Unexpand()
	expected := Introduce(test.Syntax("(#%top id)"))
	test.AssertSyntaxEqual(t, expected, actual)
}

func TestTopForm_CpsTransform(t *testing.T) {
	id := test.Identifier("id")
	location := common.NewLocation()
	ctx := common.NewCpsTransformContext([]common.Global{{id, location}})
	ctx.Add(id)
	coreForm := TopForm{Id: id, Mark: common.NewMark()}
	expected := NewTop(location)
	testCpsTransform(t, ctx, coreForm, expected)
}

func TestTopForm_CpsTransformOutOfContext(t *testing.T) {
	ctx := common.NewCpsTransformContext([]common.Global{})
	id := test.Identifier("id")
	coreForm := TopForm{Id: id, Mark: common.NewMark()}
	testCpsTransformError(t, ctx, coreForm, "cps transform: id not in context")
}
