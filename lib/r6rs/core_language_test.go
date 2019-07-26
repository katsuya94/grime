package r6rs_test

import (
	"testing"

	"github.com/katsuya94/grime/common"
	. "github.com/katsuya94/grime/lib/r6rs"
	"github.com/katsuya94/grime/test"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestCoreLanguageLiteral(t *testing.T) {
	syntax := Introduce(test.Syntax("(#%literal #t)"))
	expander := NewCoreExpander()
	actual, err := expander.Expand(syntax, CoreEnvironment)
	require.NoError(t, err)
	expected := LiteralForm{Datum: common.Boolean(true)}
	assert.Equal(t, expected, actual)
}

func TestCoreLanguageReference(t *testing.T) {
	syntax := Introduce(test.Syntax("(#%reference id)"))
	expander := NewCoreExpander()
	actual, err := expander.Expand(syntax, CoreEnvironment)
	require.NoError(t, err)
	id := Introduce(test.Syntax("id")).IdentifierOrDie()
	expected := ReferenceForm{Id: id}
	assert.Equal(t, expected, actual)
}

func TestCoreLanguageLambda(t *testing.T) {
	syntax := Introduce(test.Syntax("(#%lambda (id) (#%literal #t))"))
	expander := NewCoreExpander()
	actual, err := expander.Expand(syntax, CoreEnvironment)
	require.NoError(t, err)
	id := Introduce(test.Syntax("id")).IdentifierOrDie()
	expected := LambdaForm{
		Formals: []common.Identifier{id},
		Inner:   LiteralForm{common.Boolean(true)},
	}
	assert.Equal(t, expected, actual)
}

func TestCoreLanguageApplication(t *testing.T) {
	syntax := Introduce(test.Syntax("(#%application (#%reference id) (#%literal #t))"))
	expander := NewCoreExpander()
	actual, err := expander.Expand(syntax, CoreEnvironment)
	require.NoError(t, err)
	id := Introduce(test.Syntax("id")).IdentifierOrDie()
	expected := ApplicationForm{
		Procedure: ReferenceForm{Id: id},
		Arguments: []CoreForm{LiteralForm{common.Boolean(true)}},
	}
	assert.Equal(t, expected, actual)
}
