package r6rs_base_test

import (
	"testing"

	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/lib/r6rs"
	. "github.com/katsuya94/grime/lib/r6rs_base"
	"github.com/katsuya94/grime/test"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func testBaseLanguage(t *testing.T, syntax common.Syntax, env common.Environment, expectedSrc string) {
	actual, err := Expand(syntax, env)
	require.NoError(t, err)
	expected := r6rs.Introduce(test.Syntax(expectedSrc))
	test.AssertCoreSyntaxEqual(t, expected, actual.Unexpand())
}

func testBaseLanguageError(t *testing.T, syntax common.Syntax, env common.Environment, errMsg string) {
	_, err := Expand(syntax, env)
	assert.EqualError(t, err, errMsg)
}

func TestBaseLanguageLiteral(t *testing.T) {
	syntax := Introduce(test.Syntax("#t"))
	env := BaseEnvironment
	expectedSrc := "(#%literal #t)"
	testBaseLanguage(t, syntax, env, expectedSrc)
}

func TestBaseLanguageIdBoundOutOfContext(t *testing.T) {
	id := Introduce(test.Syntax("id")).IdentifierOrDie()
	scope := common.NewScope()
	id, _ = common.Bind(id, scope, 0)
	env := BaseEnvironment
	syntax := common.NewSyntax(id.WrappedSyntax)
	errMsg := "out of context: id at (unknown)"
	testBaseLanguageError(t, syntax, env, errMsg)
}

func TestBaseLanguageIdBoundNonVariable(t *testing.T) {
	id := Introduce(test.Syntax("id")).IdentifierOrDie()
	scope := common.NewScope()
	id, binding := common.Bind(id, scope, 0)
	env := BaseEnvironment
	(&env).Extend(binding, common.NewSyntacticAbstraction(nil))
	syntax := common.NewSyntax(id.WrappedSyntax)
	errMsg := "non-variable identifier in expression context: id is SyntacticAbstraction{<nil>} at (unknown)"
	testBaseLanguageError(t, syntax, env, errMsg)
}

func TestBaseLanguageIdBoundInContext(t *testing.T) {
	id := Introduce(test.Syntax("id")).IdentifierOrDie()
	scope := common.NewScope()
	id, binding := common.Bind(id, scope, 0)
	env := BaseEnvironment
	(&env).Extend(binding, common.NewVariable())
	syntax := common.NewSyntax(id.WrappedSyntax)
	expectedSrc := "(#%load id)"
	testBaseLanguage(t, syntax, env, expectedSrc)
}

func TestBaseLanguageIdUnbound(t *testing.T) {
	env := BaseEnvironment
	syntax := Introduce(test.Syntax("id"))
	expectedSrc := "(#%top id)"
	testBaseLanguage(t, syntax, env, expectedSrc)
}

func TestBaseLanguageLambda(t *testing.T) {
	env := BaseEnvironment
	syntax := Introduce(test.Syntax("(lambda (id) #t)"))
	expectedSrc := "(#%lambda (id) (#%sequence (#%literal #t)))"
	testBaseLanguage(t, syntax, env, expectedSrc)
}

func TestBaseLanguageBegin(t *testing.T) {
	env := BaseEnvironment
	syntax := Introduce(test.Syntax("(begin #t #f)"))
	expectedSrc := "(#%sequence (#%literal #t) (#%literal #f))"
	testBaseLanguage(t, syntax, env, expectedSrc)
}

func TestBaseLanguageBeginEmpty(t *testing.T) {
	env := BaseEnvironment
	syntax := Introduce(test.Syntax("(begin)"))
	errMsg := "unexpected final form"
	testBaseLanguageError(t, syntax, env, errMsg)
}

func TestBaseLanguageBeginNested(t *testing.T) {
	env := BaseEnvironment
	syntax := Introduce(test.Syntax("(begin (begin #t #f))"))
	expectedSrc := "(#%sequence (#%literal #t) (#%literal #f))"
	testBaseLanguage(t, syntax, env, expectedSrc)
}

func TestBaseLanguageBeginNestedEmpty(t *testing.T) {
	env := BaseEnvironment
	syntax := Introduce(test.Syntax("(begin (begin))"))
	errMsg := "unexpected final form"
	testBaseLanguageError(t, syntax, env, errMsg)
}

func TestBaseLanguageDefineSyntaxExpressionContext(t *testing.T) {
	env := BaseEnvironment
	syntax := Introduce(test.Syntax("(define-syntax id (lambda (stx) stx))"))
	errMsg := "body form in expression context: define-syntax at (unknown)"
	testBaseLanguageError(t, syntax, env, errMsg)
}

func TestBaseLanguageDefineSyntax(t *testing.T) {
	env := BaseEnvironment
	syntax := Introduce(test.Syntax("(begin (define-syntax id (lambda (stx) (syntax #t))) (id))"))
	syntax = syntax.Push(BaseScope, 1)
	expectedSrc := "(#%sequence (#%literal #t))"
	testBaseLanguage(t, syntax, env, expectedSrc)
}
