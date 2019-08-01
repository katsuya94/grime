package r6rs_test

import (
	"testing"

	"github.com/katsuya94/grime/common"
	. "github.com/katsuya94/grime/lib/r6rs"
	"github.com/katsuya94/grime/test"
)

func TestApplicationForm_Unexpand(t *testing.T) {
	id := Introduce(test.Syntax("id")).IdentifierOrDie()
	coreForm := ApplicationForm{
		Procedure: ReferenceForm{Id: id},
		Arguments: []CoreForm{LiteralForm{common.Boolean(true)}},
	}
	actual := coreForm.Unexpand()
	expected := Introduce(test.Syntax("(#%application (#%reference id) (#%literal #t))"))
	test.AssertSyntaxEqual(t, expected, actual)
}

func TestApplicationForm_CpsTransform(t *testing.T) {
	ctx := NewCpsTransformContext()
	procExpression := test.NewVoidExpression()
	argExpression := test.NewVoidExpression()
	coreForm := ApplicationForm{
		Procedure: spyForm{cpsTransform: func(ctx CpsTransformContext) (common.Expression, error) {
			return procExpression, nil
		}},
		Arguments: []CoreForm{
			spyForm{cpsTransform: func(ctx CpsTransformContext) (common.Expression, error) {
				return argExpression, nil
			}},
		},
	}
	expected := NewApplication(procExpression, argExpression)
	testCpsTransform(t, ctx, coreForm, expected)
}
