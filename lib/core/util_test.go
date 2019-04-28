package core_test

import (
	"testing"

	"github.com/stretchr/testify/require"

	"github.com/katsuya94/grime/common"
	. "github.com/katsuya94/grime/lib/core"
	"github.com/katsuya94/grime/read"
)

func expandNever(Compiler, common.Syntax) (common.Syntax, bool, error) {
	return common.Syntax{}, false, nil
}

func expressionCompileIdentity(_ Compiler, form common.Syntax) (common.Expression, error) {
	return form.Datum().(common.Expression), nil
}

func testProgram(t *testing.T, source string, expected common.Datum) {
	syntaxes, nullSourceLocationTree := read.MustReadSyntaxes(source)
	body := common.Body(nullSourceLocationTree, syntaxes...)
	scope := common.NewScope()
	for name, location := range Bindings[0] {
		scope.Set(common.NewIdentifier(name), location)
	}
	body = body.Push(scope, common.LEXICAL)
	expression, err := Compile(body, scope)
	require.NoError(t, err)
	actual, err := common.EvaluateOnce(expression)
	require.NoError(t, err)
	require.Equal(t, expected, actual)
}
