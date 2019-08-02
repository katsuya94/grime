package r6rs_base_test

import (
	"testing"

	"github.com/katsuya94/grime/lib/r6rs"
	. "github.com/katsuya94/grime/lib/r6rs_base"
	"github.com/katsuya94/grime/test"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestBaseLanguageLambda(t *testing.T) {
	syntax := Introduce(test.Syntax("(lambda (id) #t)"))
	expander := NewBaseExpander()
	actual, err := expander.Expand(syntax, BaseEnvironment)
	require.NoError(t, err)
	// expected := r6rs.Introduce(test.Syntax("(#%lambda (id) (#%begin (#%literal #t)))"))
	expected := r6rs.Introduce(test.Syntax("(#%lambda (id) (#%literal #t))"))
	assert.Equal(t, expected, actual.Unexpand())
}
