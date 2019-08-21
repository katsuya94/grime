package r6rs_base_test

import (
	"testing"

	"github.com/katsuya94/grime/lib/r6rs"
	. "github.com/katsuya94/grime/lib/r6rs_base"
	"github.com/katsuya94/grime/test"
	"github.com/stretchr/testify/require"
)

func TestBaseLanguageLiteral(t *testing.T) {
	syntax := Introduce(test.Syntax("#t"))
	expander := BaseExpander{}
	actual, err := expander.Expand(syntax, BaseEnvironment)
	require.NoError(t, err)
	expected := r6rs.Introduce(test.Syntax("(#%literal #t)"))
	test.AssertCoreSyntaxEqual(t, expected, actual.Unexpand())
}

func TestBaseLanguageIdUnbound(t *testing.T) {
	syntax := Introduce(test.Syntax("id"))
	expander := BaseExpander{}
	actual, err := expander.Expand(syntax, BaseEnvironment)
	require.NoError(t, err)
	expected := r6rs.Introduce(test.Syntax("(#%top id)"))
	test.AssertCoreSyntaxEqual(t, expected, actual.Unexpand())
}

func TestBaseLanguageLambda(t *testing.T) {
	syntax := Introduce(test.Syntax("(lambda (id) #t)"))
	expander := BaseExpander{}
	actual, err := expander.Expand(syntax, BaseEnvironment)
	require.NoError(t, err)
	expected := r6rs.Introduce(test.Syntax("(#%lambda (id) (#%sequence (#%literal #t)))"))
	test.AssertCoreSyntaxEqual(t, expected, actual.Unexpand())
}

func TestBaseLanguageBegin(t *testing.T) {
	syntax := Introduce(test.Syntax("(begin #t #f)"))
	expander := BaseExpander{}
	actual, err := expander.Expand(syntax, BaseEnvironment)
	require.NoError(t, err)
	expected := r6rs.Introduce(test.Syntax("(#%sequence (#%literal #t)(#%literal #f))"))
	test.AssertCoreSyntaxEqual(t, expected, actual.Unexpand())
}
