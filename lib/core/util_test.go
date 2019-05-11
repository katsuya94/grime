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

func expressionCompileIdentity(_ Compiler, form common.Syntax, _ *common.FrameTemplate) (common.Expression, error) {
	return form.Datum().(common.Expression), nil
}

func testProgram(t *testing.T, source string, expected common.Datum) {
	syntaxes, nullSourceLocationTree := read.MustReadSyntaxes(source)
	body := common.Body(nullSourceLocationTree, syntaxes...)
	scope := common.NewScope()
	for name, binding := range Bindings[0] {
		scope.Set(common.NewIdentifier(name), binding)
	}
	body = body.Push(scope, common.LEXICAL, false)
	frameTemplate := common.NewFrameTemplate()
	expression, err := NewCompiler().Compile(body, scope, &frameTemplate)
	require.NoError(t, err)
	frame := frameTemplate.Instantiate()
	actual, err := common.Evaluate(common.NewStack(frame), expression)
	require.NoError(t, err)
	require.Equal(t, expected, actual)
}
