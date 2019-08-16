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
	expander := NewCoreExpanderWithMarks(marks)
	actual, err := expander.Expand(syntax, CoreEnvironment)
	require.NoError(t, err)
	assert.Equal(t, expected, actual)
}

func TestCoreLanguageLiteral(t *testing.T) {
	marks := make([]common.M, 1)
	src := "(#%literal #t)"
	expected := LiteralForm{Datum: common.Boolean(true), Mark: &marks[0]}
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
	src := "(#%lambda (id) (#%literal #t))"
	id := Introduce(test.Syntax("id")).IdentifierOrDie()
	expected := LambdaForm{
		Formals: []common.Identifier{id},
		Inner:   LiteralForm{Datum: common.Boolean(true), Mark: &marks[1]},
		Mark:    &marks[0],
	}
	testCoreLanguage(t, marks, src, expected)
}

func TestCoreLanguageApplication(t *testing.T) {
	marks := make([]common.M, 3)
	src := "(#%application (#%load id) (#%literal #t))"
	id := Introduce(test.Syntax("id")).IdentifierOrDie()
	expected := ApplicationForm{
		Procedure: LoadForm{Id: id, Mark: &marks[1]},
		Arguments: []CoreForm{LiteralForm{Datum: common.Boolean(true), Mark: &marks[2]}},
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
	src := "(#%sequence (#%literal #t) (#%literal #f))"
	expected := SequenceForm{
		Forms: []CoreForm{
			LiteralForm{Datum: common.Boolean(true), Mark: &marks[1]},
			LiteralForm{Datum: common.Boolean(false), Mark: &marks[2]},
		},
		Mark: &marks[0],
	}
	testCoreLanguage(t, marks, src, expected)
}
