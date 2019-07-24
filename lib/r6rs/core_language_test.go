package r6rs_test

import (
	"testing"

	"github.com/katsuya94/grime/common"
	. "github.com/katsuya94/grime/lib/r6rs"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestCoreLanguageLiteral(t *testing.T) {
	source := "(#%literal id)"
	syntax := Introduce(read.MustReadSyntax(source))
	expander := NewCoreExpander()
	ctx := NewExpansionContext(expander)
	coreForm, err := expander.Expand(ctx, syntax)
	require.NoError(t, err)
	assert.Equal(coreForm, LiteralForm{common.Symbol("id")})
}
