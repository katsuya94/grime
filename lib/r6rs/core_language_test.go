package r6rs_test

import (
	"testing"

	"github.com/katsuya94/grime/common"
	. "github.com/katsuya94/grime/lib/r6rs"
	"github.com/katsuya94/grime/test"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func testCoreLanguage(t *testing.T, marks []common.M, src string, expected CoreForm) {
	syntax := Introduce(test.Syntax(src))
	actual, err := ExpandWithMarks(syntax, marks)
	require.NoError(t, err)
	assert.Equal(t, expected, actual)
}

func TestCoreLanguageQuote(t *testing.T) {
	marks := make([]common.M, 1)
	src := "(quote #t)"
	expected := QuoteForm{Datum: common.Boolean(true), Mark: &marks[0]}
	testCoreLanguage(t, marks, src, expected)
}

func TestCoreLanguageLoad(t *testing.T) {
	marks := make([]common.M, 1)
	src := "(#%load id)"
	id := Introduce(test.Syntax("id")).IdentifierOrDie()
	expected := LoadForm{Id: id, Mark: &marks[0]}
	testCoreLanguage(t, marks, src, expected)
}

func TestCoreLanguageLambda(t *testing.T) {
	marks := make([]common.M, 2)
	src := "(#%lambda (id) (quote #t))"
	id := Introduce(test.Syntax("id")).IdentifierOrDie()
	expected := LambdaForm{
		Formals: []common.Identifier{id},
		Inner:   QuoteForm{Datum: common.Boolean(true), Mark: &marks[1]},
		Mark:    &marks[0],
	}
	testCoreLanguage(t, marks, src, expected)
}

func TestCoreLanguageApplication(t *testing.T) {
	marks := make([]common.M, 3)
	src := "(#%application (#%load id) (quote #t))"
	id := Introduce(test.Syntax("id")).IdentifierOrDie()
	expected := ApplicationForm{
		Procedure: LoadForm{Id: id, Mark: &marks[1]},
		Arguments: []CoreForm{QuoteForm{Datum: common.Boolean(true), Mark: &marks[2]}},
		Mark:      &marks[0],
	}
	testCoreLanguage(t, marks, src, expected)
}

func TestCoreLanguageTop(t *testing.T) {
	marks := make([]common.M, 1)
	src := "(#%top id)"
	id := Introduce(test.Syntax("id")).IdentifierOrDie()
	expected := TopForm{Id: id, Mark: &marks[0]}
	testCoreLanguage(t, marks, src, expected)
}

func TestCoreLanguageSequence(t *testing.T) {
	marks := make([]common.M, 3)
	src := "(#%sequence (quote #t) (quote #f))"
	expected := SequenceForm{
		Forms: []CoreForm{
			QuoteForm{Datum: common.Boolean(true), Mark: &marks[1]},
			QuoteForm{Datum: common.Boolean(false), Mark: &marks[2]},
		},
		Mark: &marks[0],
	}
	testCoreLanguage(t, marks, src, expected)
}

func TestCoreLanguageSyntax(t *testing.T) {
	marks := make([]common.M, 1)
	src := "(syntax #t)"
	template := Introduce(test.Syntax("#t"))
	expected := SyntaxForm{Template: template, Mark: &marks[0]}
	testCoreLanguage(t, marks, src, expected)
}
