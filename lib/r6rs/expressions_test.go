package r6rs_test

import (
	"testing"

	"github.com/katsuya94/grime/common"
	. "github.com/katsuya94/grime/lib/r6rs"
	"github.com/katsuya94/grime/test"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func testEvaluate(t *testing.T, src string, expected common.Datum) {
	syntax := Introduce(test.Syntax(src))
	expander := NewCoreExpander()
	coreForm, err := expander.Expand(syntax, CoreEnvironment)
	require.NoError(t, err)
	expression := CpsTransform(coreForm)
	actual, err := test.Evaluate(expression)
	require.NoError(t, err)
	assert.Equal(t, expected, actual)
}

func TestLiteral_Evaluate(t *testing.T) {
	testEvaluate(t, "(#%literal #t)", common.Boolean(true))
}
