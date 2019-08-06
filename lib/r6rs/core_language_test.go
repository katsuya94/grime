package r6rs_test

import (
	"testing"

	"github.com/katsuya94/grime/common"
	. "github.com/katsuya94/grime/lib/r6rs"
	"github.com/katsuya94/grime/test"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func testCoreLanguage(t *testing.T, src string, expected CoreForm) {
	syntax := Introduce(test.Syntax(src))
	expander := NewSelfReferentialCoreExpander()
	actual, err := expander.Expand(syntax, CoreEnvironment)
	require.NoError(t, err)
	assert.Equal(t, expected, actual)
}

func TestCoreLanguageLiteral(t *testing.T) {
	src := "(#%literal #t)"
	expected := LiteralForm{Datum: common.Boolean(true)}
	testCoreLanguage(t, src, expected)
}

func TestCoreLanguageReference(t *testing.T) {
	src := "(#%reference id)"
	id := Introduce(test.Syntax("id")).IdentifierOrDie()
	expected := ReferenceForm{Id: id}
	testCoreLanguage(t, src, expected)
}

func TestCoreLanguageLambda(t *testing.T) {
	src := "(#%lambda (id) (#%literal #t))"
	id := Introduce(test.Syntax("id")).IdentifierOrDie()
	expected := LambdaForm{
		Formals: []common.Identifier{id},
		Inner:   LiteralForm{common.Boolean(true)},
	}
	testCoreLanguage(t, src, expected)
}

func TestCoreLanguageApplication(t *testing.T) {
	src := "(#%application (#%reference id) (#%literal #t))"
	id := Introduce(test.Syntax("id")).IdentifierOrDie()
	expected := ApplicationForm{
		Procedure: ReferenceForm{Id: id},
		Arguments: []CoreForm{LiteralForm{common.Boolean(true)}},
	}
	testCoreLanguage(t, src, expected)
}

func TestCoreLanguageTop(t *testing.T) {
	src := "(#%top id)"
	id := Introduce(test.Syntax("id")).IdentifierOrDie()
	expected := TopForm{Id: id}
	testCoreLanguage(t, src, expected)
}
