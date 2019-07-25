package r6rs_test

import (
	"testing"

	"github.com/katsuya94/grime/common"
	. "github.com/katsuya94/grime/lib/r6rs"
	"github.com/katsuya94/grime/read"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestCoreLanguageLiteral(t *testing.T) {
	source := "(#%literal id)"
	syntax := Introduce(read.MustReadSyntax(source))
	expander := NewCoreExpander()
	coreForm, err := expander.Expand(syntax, CoreEnvironment)
	require.NoError(t, err)
	assert.Equal(t, coreForm, LiteralForm{common.Symbol("id")})
}
