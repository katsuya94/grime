package core_test

import (
	"testing"

	"github.com/stretchr/testify/require"

	"github.com/katsuya94/grime/common"
	. "github.com/katsuya94/grime/lib/core"
	"github.com/katsuya94/grime/read"
)

func expandNever(Compiler, common.Syntax, common.Stack) (common.Syntax, bool, error) {
	return common.Syntax{}, false, nil
}

func expressionCompileIdentity(_ Compiler, form common.Syntax, _ *common.FrameTemplate, _ common.Stack) (common.Expression, error) {
	return form.Datum().(common.Expression), nil
}

func testProgram(t *testing.T, source string, expected common.Datum) {
	syntaxes, nullSourceLocationTree := read.MustReadSyntaxes(source)
	body := common.Body(nullSourceLocationTree, syntaxes...)
	scopeSet := common.NewScopeSet()
	frame := common.NewFrame()
	err := Bindings.Load([]int{0}, scopeSet, frame, common.IdentifierTransformerFactoryAll)
	require.NoError(t, err)
	body = body.Push(scopeSet[0], common.LEXICAL, false)
	frameTemplate := frame.Template()
	stack := common.NewStack(frame)
	var expression common.Expression
	require.NotPanics(t, func() {
		expression, err = NewCompiler().Compile(body, scopeSet[0], &frameTemplate, stack)
	})
	require.NoError(t, err)
	frame.Grow(frameTemplate)
	var actual common.Datum
	require.NotPanics(t, func() {
		actual, err = common.Evaluate(stack, expression)
	})
	require.NoError(t, err)
	require.Equal(t, expected, actual)
}
