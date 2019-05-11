package common_test

import (
	"testing"

	. "github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/test"
	"github.com/stretchr/testify/require"
)

func TestPatternLiteral_MatchUnbound(t *testing.T) {
	pattern := test.WithLiteral(test.Identifier("literal"), test.Syntax("literal"))
	frameTemplate := NewFrameTemplate()

	compiled, patternVariableInfos, err := CompilePattern(pattern, &frameTemplate)
	require.NoError(t, err)
	require.Empty(t, patternVariableInfos)

	input := test.Syntax("literal")

	result, ok := compiled.Match(input)
	require.True(t, ok)
	require.Empty(t, result)
}

func TestPatternLiteral_MatchBound(t *testing.T) {
	bindings := test.Bindings(1)

	literal, ok := test.WithBinding(test.Identifier("literal"), &bindings[0], test.Syntax("literal")).Identifier()
	require.True(t, ok)

	pattern := test.WithLiteral(literal, test.Syntax("literal"))
	frameTemplate := NewFrameTemplate()

	compiled, patternVariableInfos, err := CompilePattern(pattern, &frameTemplate)
	require.NoError(t, err)
	require.Empty(t, patternVariableInfos)

	input := test.Syntax("different")
	input = test.WithBinding(test.Identifier("different"), &bindings[0], test.Syntax("different"))

	result, ok := compiled.Match(input)
	require.True(t, ok)
	require.Empty(t, result)
}

func TestPatternLiteral_NoMatchUnbound(t *testing.T) {
	pattern := test.WithLiteral(test.Identifier("literal"), test.Syntax("literal"))
	frameTemplate := NewFrameTemplate()

	compiled, patternVariableInfos, err := CompilePattern(pattern, &frameTemplate)
	require.NoError(t, err)
	require.Empty(t, patternVariableInfos)

	input := test.Syntax("different")

	_, ok := compiled.Match(input)
	require.False(t, ok)
}

func TestPatternLiteral_NoMatchBound(t *testing.T) {
	bindings := test.Bindings(2)

	literal, ok := test.WithBinding(test.Identifier("id"), &bindings[0], test.Syntax("id")).Identifier()
	require.True(t, ok)

	pattern := test.WithLiteral(literal, test.Syntax("id"))
	frameTemplate := NewFrameTemplate()

	compiled, patternVariableInfos, err := CompilePattern(pattern, &frameTemplate)
	require.NoError(t, err)
	require.Empty(t, patternVariableInfos)

	input := test.Syntax("id")
	input = test.WithBinding(test.Identifier("id"), &bindings[1], test.Syntax("id"))

	_, ok = compiled.Match(input)
	require.False(t, ok)
}

func TestPatternUnderscore_MatchAnything(t *testing.T) {
	pattern := test.WithBinding(test.Identifier("_"), UnderscoreKeyword, test.Syntax("_"))
	frameTemplate := NewFrameTemplate()

	compiled, patternVariableInfos, err := CompilePattern(pattern, &frameTemplate)
	require.NoError(t, err)
	require.Empty(t, patternVariableInfos)

	input := test.Syntax("anything")

	result, ok := compiled.Match(input)
	require.True(t, ok)
	require.Empty(t, result)
}

func TestPatternVariable_MatchAnything(t *testing.T) {
	pattern := test.Syntax("pattern-variable")
	frameTemplate := NewFrameTemplate()

	compiled, patternVariableInfos, err := CompilePattern(pattern, &frameTemplate)
	require.NoError(t, err)
	require.Len(t, patternVariableInfos, 1)
	patternVariableInfo := patternVariableInfos[0]
	require.True(t, patternVariableInfo.Id.Equal(test.Identifier("pattern-variable")))
	require.True(t, patternVariableInfo.PatternVariable.Nesting == 0)

	input := test.Syntax("anything")

	result, ok := compiled.Match(input)
	require.True(t, ok)
	require.Len(t, result, 1)
	require.Equal(t, test.Syntax("anything"), result[patternVariableInfo.PatternVariable])
}

func TestPatternEllipsis_MatchEmpty(t *testing.T) {
	pattern := test.WithBinding(test.Identifier("..."), EllipsisKeyword, test.Syntax("(#t ...)"))
	frameTemplate := NewFrameTemplate()

	compiled, patternVariableInfos, err := CompilePattern(pattern, &frameTemplate)
	require.NoError(t, err)
	require.Empty(t, patternVariableInfos)

	input := test.Syntax("()")

	result, ok := compiled.Match(input)
	require.True(t, ok)
	require.Empty(t, result)
}

func TestPatternEllipsis_MatchEmptyWithRestPatternVariable(t *testing.T) {
	pattern := test.WithBinding(test.Identifier("..."), EllipsisKeyword, test.Syntax("(#t ... . pattern-variable)"))
	frameTemplate := NewFrameTemplate()

	compiled, patternVariableInfos, err := CompilePattern(pattern, &frameTemplate)
	require.NoError(t, err)
	require.Len(t, patternVariableInfos, 1)
	patternVariableInfo := patternVariableInfos[0]
	require.True(t, patternVariableInfo.Id.Equal(test.Identifier("pattern-variable")))
	require.True(t, patternVariableInfo.PatternVariable.Nesting == 0)

	input := test.Syntax("anything")

	result, ok := compiled.Match(input)
	require.True(t, ok)
	require.Len(t, result, 1)
	require.Equal(t, test.Syntax("anything"), result[patternVariableInfo.PatternVariable])
}

func TestPatternEllipsis_MatchNonEmpty(t *testing.T) {
	pattern := test.WithBinding(test.Identifier("..."), EllipsisKeyword, test.Syntax("(#t ...)"))
	frameTemplate := NewFrameTemplate()

	compiled, patternVariableInfos, err := CompilePattern(pattern, &frameTemplate)
	require.NoError(t, err)
	require.Empty(t, patternVariableInfos)

	input := test.Syntax("(#t #t)")

	result, ok := compiled.Match(input)
	require.True(t, ok)
	require.Empty(t, result)
}

func TestPatternEllipsis_MatchNonEmptyWithPatternVariable(t *testing.T) {
	pattern := test.WithBinding(test.Identifier("..."), EllipsisKeyword, test.Syntax("(pattern-variable ...)"))
	frameTemplate := NewFrameTemplate()

	compiled, patternVariableInfos, err := CompilePattern(pattern, &frameTemplate)
	require.NoError(t, err)
	require.Len(t, patternVariableInfos, 1)
	patternVariableInfo := patternVariableInfos[0]
	require.True(t, patternVariableInfo.Id.Equal(test.Identifier("pattern-variable")))
	require.True(t, patternVariableInfo.PatternVariable.Nesting == 1)

	input := test.Syntax("(anything everything)")

	result, ok := compiled.Match(input)
	require.True(t, ok)
	require.Len(t, result, 1)
	matches := result[patternVariableInfo.PatternVariable].([]interface{})
	require.Len(t, matches, 2)
	require.Equal(t, test.Syntax("anything"), matches[0])
	require.Equal(t, test.Syntax("everything"), matches[1])
}

func TestPatternEllipsis_MatchNonEmptyEarlyRest(t *testing.T) {
	pattern := test.WithBinding(test.Identifier("..."), EllipsisKeyword, test.Syntax("(#t ... #t)"))
	frameTemplate := NewFrameTemplate()

	compiled, patternVariableInfos, err := CompilePattern(pattern, &frameTemplate)
	require.NoError(t, err)
	require.Empty(t, patternVariableInfos)

	input := test.Syntax("(#t #t)")

	result, ok := compiled.Match(input)
	require.True(t, ok)
	require.Empty(t, result)
}

func TestPatternEllipsis_NoMatchBadRest(t *testing.T) {
	pattern := test.WithBinding(test.Identifier("..."), EllipsisKeyword, test.Syntax("(#t ...)"))
	frameTemplate := NewFrameTemplate()

	compiled, patternVariableInfos, err := CompilePattern(pattern, &frameTemplate)
	require.NoError(t, err)
	require.Empty(t, patternVariableInfos)

	input := test.Syntax("#t")

	_, ok := compiled.Match(input)
	require.False(t, ok)
}

func TestPatternEllipsis_NoMatchBadSubPattern(t *testing.T) {
	pattern := test.WithBinding(test.Identifier("..."), EllipsisKeyword, test.Syntax("(#t ...)"))
	frameTemplate := NewFrameTemplate()

	compiled, patternVariableInfos, err := CompilePattern(pattern, &frameTemplate)
	require.NoError(t, err)
	require.Empty(t, patternVariableInfos)

	input := test.Syntax("(#f)")

	_, ok := compiled.Match(input)
	require.False(t, ok)
}

func TestPatternPair_Match(t *testing.T) {
	pattern := test.Syntax("(#t . #t)")
	frameTemplate := NewFrameTemplate()

	compiled, patternVariableInfos, err := CompilePattern(pattern, &frameTemplate)
	require.NoError(t, err)
	require.Empty(t, patternVariableInfos)

	input := test.Syntax("(#t . #t)")

	result, ok := compiled.Match(input)
	require.True(t, ok)
	require.Empty(t, result)
}

func TestPatternPair_NoMatchBadFirst(t *testing.T) {
	pattern := test.Syntax("(#t . #t)")
	frameTemplate := NewFrameTemplate()

	compiled, patternVariableInfos, err := CompilePattern(pattern, &frameTemplate)
	require.NoError(t, err)
	require.Empty(t, patternVariableInfos)

	input := test.Syntax("(#f . #t)")

	_, ok := compiled.Match(input)
	require.False(t, ok)
}

func TestPatternPair_NoMatchBadRest(t *testing.T) {
	pattern := test.Syntax("(#t . #t)")
	frameTemplate := NewFrameTemplate()

	compiled, patternVariableInfos, err := CompilePattern(pattern, &frameTemplate)
	require.NoError(t, err)
	require.Empty(t, patternVariableInfos)

	input := test.Syntax("(#t . #f)")

	_, ok := compiled.Match(input)
	require.False(t, ok)
}

func TestPatternDatum_Match(t *testing.T) {
	pattern := test.Syntax("#t")
	frameTemplate := NewFrameTemplate()

	compiled, patternVariableInfos, err := CompilePattern(pattern, &frameTemplate)
	require.NoError(t, err)
	require.Empty(t, patternVariableInfos)

	input := test.Syntax("#t")

	result, ok := compiled.Match(input)
	require.True(t, ok)
	require.Empty(t, result)
}

func TestPatternDatum_NoMatch(t *testing.T) {
	pattern := test.Syntax("#t")
	frameTemplate := NewFrameTemplate()

	compiled, patternVariableInfos, err := CompilePattern(pattern, &frameTemplate)
	require.NoError(t, err)
	require.Empty(t, patternVariableInfos)

	input := test.Syntax("#f")

	_, ok := compiled.Match(input)
	require.False(t, ok)
}
