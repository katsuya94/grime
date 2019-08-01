package r6rs_test

import (
	"testing"

	"github.com/katsuya94/grime/common"
	. "github.com/katsuya94/grime/lib/r6rs"
	"github.com/katsuya94/grime/test"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func testEvaluate(t *testing.T, src string, boundIds []string, expected common.Datum) {
	scope := common.NewScope()
	syntax := Introduce(test.Syntax(src)).Push(scope, 0)
	env := CoreEnvironment
	for _, boundId := range boundIds {
		binding := common.NewBinding()
		scope.Add(test.Identifier(boundId), binding)
		(&env).Extend(binding, common.NewVariable())
	}
	expander := NewCoreExpander()
	coreForm, err := expander.Expand(syntax, CoreEnvironment)
	require.NoError(t, err)
	expression, err := CpsTransform(coreForm)
	require.NoError(t, err)
	actual, err := test.Evaluate(expression)
	require.NoError(t, err)
	assert.Equal(t, expected, actual)
}

func TestLiteral_Evaluate(t *testing.T) {
	src := "(#%literal #t)"
	boundIdNames := []string{}
	expected := common.Boolean(true)
	testEvaluate(t, src, boundIdNames, expected)
}

func TestLambdaReference_Evaluate(t *testing.T) {
	src := "(#%application (#%lambda () (#%literal #t)))"
	boundIdNames := []string{}
	expected := common.Boolean(true)
	testEvaluate(t, src, boundIdNames, expected)
}

func TestApplication_Evaluate(t *testing.T) {
	src := "(#%application (#%lambda (id) (#%reference id)) (#%literal #t))"
	boundIdNames := []string{"id"}
	expected := common.Boolean(true)
	testEvaluate(t, src, boundIdNames, expected)
}
