package r6rs_test

import (
	"testing"

	"github.com/katsuya94/grime/common"
	. "github.com/katsuya94/grime/lib/r6rs"
	"github.com/katsuya94/grime/test"
	"github.com/stretchr/testify/assert"
)

func TestLambdaForm_Unexpand(t *testing.T) {
	id := Introduce(test.Syntax("id")).IdentifierOrDie()
	coreForm := LambdaForm{
		Formals: []common.Identifier{id},
		Inner:   LiteralForm{Datum: common.Boolean(true)},
	}
	actual := coreForm.Unexpand()
	expected := Introduce(test.Syntax("(#%lambda (id) (#%literal #t))"))
	test.AssertSyntaxEqual(t, expected, actual)
}

func TestLambdaForm_CpsTransform(t *testing.T) {
	ctx := NewCpsTransformContext([]Global{})
	id := test.Identifier("id")
	expression := test.NewVoidExpression()
	coreForm := LambdaForm{
		Formals: []common.Identifier{id},
		Inner: spyForm{cpsTransform: func(ctx CpsTransformContext) (common.Expression, error) {
			assert.Equal(t, ctx.IndexOrDie(id), 0)
			return expression, nil
		}},
	}
	expected := NewLambda(expression, 1)
	testCpsTransform(t, ctx, coreForm, expected)
}
