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

func testBaseLanguage(t *testing.T, env common.Environment, syntax common.Syntax, expectedSrc string) {
	actual, err := Expand(env, syntax)
	require.NoError(t, err)
	expected := r6rs.Introduce(test.Syntax(expectedSrc))
	test.AssertCoreSyntaxEqual(t, expected, actual.(r6rs.Unexpander).Unexpand())
}

func testBaseLanguageError(t *testing.T, env common.Environment, syntax common.Syntax, errMsg string) {
	_, err := Expand(env, syntax)
	assert.EqualError(t, err, errMsg)
}

func TestBaseLanguageLiteral(t *testing.T) {
	syntax := Introduce(test.Syntax("#t"))
	env := BaseEnvironment
	expectedSrc := "(quote #t)"
	testBaseLanguage(t, env, syntax, expectedSrc)
}

func TestBaseLanguageIdBoundOutOfContext(t *testing.T) {
	id := Introduce(test.Syntax("id")).IdentifierOrDie()
	scope := common.NewScope()
	id, _ = common.Bind(id, scope)
	env := BaseEnvironment
	syntax := common.NewSyntax(id.WrappedSyntax)
	errMsg := "out of context: id at (unknown)"
	testBaseLanguageError(t, env, syntax, errMsg)
}

func TestBaseLanguageIdBoundNonVariable(t *testing.T) {
	id := Introduce(test.Syntax("id")).IdentifierOrDie()
	scope := common.NewScope()
	id, binding := common.Bind(id, scope)
	env := BaseEnvironment
	(&env).Extend(binding, common.NewSyntacticAbstraction(nil))
	syntax := common.NewSyntax(id.WrappedSyntax)
	errMsg := "non-variable identifier in expression context: id is common.SyntacticAbstraction at (unknown)"
	testBaseLanguageError(t, env, syntax, errMsg)
}

func TestBaseLanguageIdBoundInContext(t *testing.T) {
	id := Introduce(test.Syntax("id")).IdentifierOrDie()
	scope := common.NewScope()
	id, binding := common.Bind(id, scope)
	env := BaseEnvironment
	(&env).Extend(binding, common.NewVariable())
	syntax := common.NewSyntax(id.WrappedSyntax)
	expectedSrc := "(#%load id)"
	testBaseLanguage(t, env, syntax, expectedSrc)
}

func TestBaseLanguageIdUnbound(t *testing.T) {
	env := BaseEnvironment
	syntax := Introduce(test.Syntax("id"))
	expectedSrc := "(#%top id)"
	testBaseLanguage(t, env, syntax, expectedSrc)
}

func TestBaseLanguageLambda(t *testing.T) {
	env := BaseEnvironment
	syntax := Introduce(test.Syntax("(lambda (id) #t)"))
	expectedSrc := "(#%lambda (id) (#%sequence (quote #t)))"
	testBaseLanguage(t, env, syntax, expectedSrc)
}

func TestBaseLanguageBegin(t *testing.T) {
	env := BaseEnvironment
	syntax := Introduce(test.Syntax("(begin #t #f)"))
	expectedSrc := "(#%sequence (quote #t) (quote #f))"
	testBaseLanguage(t, env, syntax, expectedSrc)
}

func TestBaseLanguageBeginEmpty(t *testing.T) {
	env := BaseEnvironment
	syntax := Introduce(test.Syntax("(begin)"))
	errMsg := "unexpected final form"
	testBaseLanguageError(t, env, syntax, errMsg)
}

func TestBaseLanguageBeginNested(t *testing.T) {
	env := BaseEnvironment
	syntax := Introduce(test.Syntax("(begin (begin #t #f))"))
	expectedSrc := "(#%sequence (quote #t) (quote #f))"
	testBaseLanguage(t, env, syntax, expectedSrc)
}

func TestBaseLanguageBeginNestedEmpty(t *testing.T) {
	env := BaseEnvironment
	syntax := Introduce(test.Syntax("(begin (begin))"))
	errMsg := "unexpected final form"
	testBaseLanguageError(t, env, syntax, errMsg)
}

func TestBaseLanguageDefineSyntaxExpressionContext(t *testing.T) {
	env := BaseEnvironment
	syntax := Introduce(test.Syntax("(define-syntax id (lambda (stx) stx))"))
	errMsg := "body form in expression context: define-syntax at (unknown)"
	testBaseLanguageError(t, env, syntax, errMsg)
}

func TestBaseLanguageDefineSyntax(t *testing.T) {
	env := BaseEnvironment
	syntax := Introduce(test.Syntax("(begin (define-syntax id (lambda (stx) (syntax #t))) (id))"))
	expectedSrc := "(#%sequence (quote #t))"
	testBaseLanguage(t, env, syntax, expectedSrc)
}

func TestBaseLanguageSyntaxCase(t *testing.T) {
	env := BaseEnvironment
	syntax := Introduce(test.Syntax(`
(begin
 	(define-syntax or
		(lambda (stx)
			(syntax-case stx ()
				((_)
					(syntax #f))
				((_ e)
					(syntax e))
				((_ e1 e2 e3 ...)
					(syntax ((lambda (t) (if t t (or e2 e3 ...))) e1))))))
	(or #f (quote id)))`))
	expectedSrc := `
(#%sequence
	(#%application
		(#%lambda (t)
			(#%sequence
				(if (#%load t)
					(#%load t)
					(quote id))))
		(quote #f)))`
	testBaseLanguage(t, env, syntax, expectedSrc)
}
