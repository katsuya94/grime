package r6rs_test

import (
	"testing"

	"github.com/katsuya94/grime/common"
	. "github.com/katsuya94/grime/lib/r6rs"
	"github.com/katsuya94/grime/test"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func testEvaluate(t *testing.T, src string, globals []Global, expected common.Datum) {
	scope := common.NewScope()
	syntax := Introduce(test.Syntax(src)).Push(scope, 0)
	expander := NewCoreExpander()
	coreForm, err := expander.Expand(syntax, CoreEnvironment)
	require.NoError(t, err)
	ctx := NewCpsTransformContext(globals)
	expression, err := coreForm.CpsTransform(ctx)
	require.NoError(t, err)
	actual, err := test.Evaluate(expression)
	require.NoError(t, err)
	assert.Equal(t, expected, actual)
}

func TestLiteral_Evaluate(t *testing.T) {
	src := "(#%literal #t)"
	globals := []Global{}
	expected := common.Boolean(true)
	testEvaluate(t, src, globals, expected)
}

func TestLambdaLoad_Evaluate(t *testing.T) {
	src := "(#%application (#%lambda (id) (#%load id)) (#%literal #t))"
	globals := []Global{}
	expected := common.Boolean(true)
	testEvaluate(t, src, globals, expected)
}

func TestApplication_Evaluate(t *testing.T) {
	src := "(#%application (#%lambda () (#%literal #t)))"
	globals := []Global{}
	expected := common.Boolean(true)
	testEvaluate(t, src, globals, expected)
}

func TestTop_Evaluate(t *testing.T) {
	src := "(#%top id)"
	id := Introduce(common.NewSyntax(test.Identifier("id").WrappedSyntax)).IdentifierOrDie()
	location := common.NewLocation()
	location.Set(common.Boolean(true))
	globals := []Global{
		{
			Id:       id,
			Location: location,
		},
	}
	expected := common.Boolean(true)
	testEvaluate(t, src, globals, expected)
}

func TestSequence_Evaluate(t *testing.T) {
	src := "(#%sequence (#%literal #t) (#%literal #f))"
	globals := []Global{}
	expected := common.Boolean(false)
	testEvaluate(t, src, globals, expected)
}
