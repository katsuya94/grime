package r6rs_test

import (
	"testing"

	"github.com/katsuya94/grime/common"
	. "github.com/katsuya94/grime/lib/r6rs"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

type spyForm struct {
	unexpand     func() common.Syntax
	cpsTransform func(CpsTransformContext) (common.Expression, error)
}

func (f spyForm) Unexpand() common.Syntax {
	return f.unexpand()
}

func (f spyForm) CpsTransform(ctx CpsTransformContext) (common.Expression, error) {
	return f.cpsTransform(ctx)
}

func testCpsTransform(t *testing.T, ctx CpsTransformContext, coreForm CoreForm, expected common.Expression) {
	actual, err := coreForm.CpsTransform(ctx)
	require.NoError(t, err)
	assert.Equal(t, expected, actual)
}

func testCpsTransformError(t *testing.T, ctx CpsTransformContext, coreForm CoreForm, expected string) {
	_, err := coreForm.CpsTransform(ctx)
	assert.EqualError(t, err, expected)
}
