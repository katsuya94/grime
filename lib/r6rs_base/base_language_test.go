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

func TestBaseLanguageLiteral(t *testing.T) {
	syntax := Introduce(test.Syntax("#t"))
	expander := BaseExpander{}
	actual, err := expander.Expand(syntax, BaseEnvironment)
	require.NoError(t, err)
	expected := r6rs.Introduce(test.Syntax("(#%literal #t)"))
	test.AssertCoreSyntaxEqual(t, expected, actual.Unexpand())
}

func TestBaseLanguageIdBoundOutOfContext(t *testing.T) {
	id := Introduce(test.Syntax("id")).IdentifierOrDie()
	scope := common.NewScope()
	id, _ = common.Bind(id, scope, 0)
	syntax := common.NewSyntax(id.WrappedSyntax)
	expander := BaseExpander{}
	_, err := expander.Expand(syntax, BaseEnvironment)
	assert.EqualError(t, err, "out of context: id at (unknown)")
}

func TestBaseLanguageIdBoundNonVariable(t *testing.T) {
	id := Introduce(test.Syntax("id")).IdentifierOrDie()
	scope := common.NewScope()
	id, binding := common.Bind(id, scope, 0)
	env := BaseEnvironment
	(&env).Extend(binding, common.NewSyntacticAbstraction(nil))
	syntax := common.NewSyntax(id.WrappedSyntax)
	expander := BaseExpander{}
	_, err := expander.Expand(syntax, env)
	assert.EqualError(t, err, "non-variable identifier in value context: id is SyntacticAbstraction{<nil>} at (unknown)")
}

func TestBaseLanguageIdBoundInContext(t *testing.T) {
	id := Introduce(test.Syntax("id")).IdentifierOrDie()
	scope := common.NewScope()
	id, binding := common.Bind(id, scope, 0)
	env := BaseEnvironment
	(&env).Extend(binding, common.NewVariable())
	syntax := common.NewSyntax(id.WrappedSyntax)
	expander := BaseExpander{}
	actual, err := expander.Expand(syntax, env)
	require.NoError(t, err)
	expected := r6rs.Introduce(test.Syntax("(#%load id)"))
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

func TestBaseLanguageBeginSimple(t *testing.T) {
	syntax := Introduce(test.Syntax("(begin #t #f)"))
	expander := BaseExpander{}
	actual, err := expander.Expand(syntax, BaseEnvironment)
	require.NoError(t, err)
	expected := r6rs.Introduce(test.Syntax("(#%sequence (#%literal #t) (#%literal #f))"))
	test.AssertCoreSyntaxEqual(t, expected, actual.Unexpand())
}
