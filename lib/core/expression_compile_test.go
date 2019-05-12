package core_test

import (
	"testing"

	"github.com/katsuya94/grime/read"
	"github.com/katsuya94/grime/test"

	"github.com/stretchr/testify/require"

	"github.com/katsuya94/grime/common"
	. "github.com/katsuya94/grime/lib/core"
)

func TestExpressionCompile_Quote(t *testing.T) {
	frameTemplate := common.NewFrameTemplate()
	compiler := Compiler{Expander: expandNever}
	expression, err := ExpressionCompile(compiler, common.NewSyntax(QuoteForm{read.MustReadDatum("id")}), &frameTemplate)
	require.NoError(t, err)
	require.Equal(t, Literal{read.MustReadDatum("id")}, expression)
	require.Equal(t, 0, frameTemplate.Size())
}

func TestExpressionCompile_SyntaxBoolean(t *testing.T) {
	frameTemplate := common.NewFrameTemplate()
	compiler := Compiler{Expander: expandNever}
	template := test.Syntax("#f")
	expression, err := ExpressionCompile(compiler, common.NewSyntax(SyntaxForm{template}), &frameTemplate)
	require.NoError(t, err)
	require.Equal(t, SyntaxTemplate{template.Datum(), nil}, expression)
	require.Equal(t, 0, frameTemplate.Size())
}

func TestExpressionCompile_SyntaxNumber(t *testing.T) {
	frameTemplate := common.NewFrameTemplate()
	compiler := Compiler{Expander: expandNever}
	template := test.Syntax("123")
	expression, err := ExpressionCompile(compiler, common.NewSyntax(SyntaxForm{template}), &frameTemplate)
	require.NoError(t, err)
	require.Equal(t, SyntaxTemplate{test.Syntax("123").Datum(), nil}, expression)
	require.Equal(t, SyntaxTemplate{template.Datum(), nil}, expression)
	require.Equal(t, 0, frameTemplate.Size())
}

func TestExpressionCompile_SyntaxCharacter(t *testing.T) {
	frameTemplate := common.NewFrameTemplate()
	compiler := Compiler{Expander: expandNever}
	template := test.Syntax(`#\x`)
	expression, err := ExpressionCompile(compiler, common.NewSyntax(SyntaxForm{template}), &frameTemplate)
	require.NoError(t, err)
	require.Equal(t, SyntaxTemplate{template.Datum(), nil}, expression)
	require.Equal(t, 0, frameTemplate.Size())
}

func TestExpressionCompile_SyntaxString(t *testing.T) {
	frameTemplate := common.NewFrameTemplate()
	compiler := Compiler{Expander: expandNever}
	template := test.Syntax(`"thing"`)
	expression, err := ExpressionCompile(compiler, common.NewSyntax(SyntaxForm{template}), &frameTemplate)
	require.NoError(t, err)
	require.Equal(t, SyntaxTemplate{template.Datum(), nil}, expression)
	require.Equal(t, 0, frameTemplate.Size())
}

func TestExpressionCompile_SyntaxNull(t *testing.T) {
	frameTemplate := common.NewFrameTemplate()
	compiler := Compiler{Expander: expandNever}
	template := test.Syntax("()")
	expression, err := ExpressionCompile(compiler, common.NewSyntax(SyntaxForm{template}), &frameTemplate)
	require.NoError(t, err)
	require.Equal(t, SyntaxTemplate{template.Datum(), nil}, expression)
	require.Equal(t, 0, frameTemplate.Size())
}

func TestExpressionCompile_SyntaxIdentifier(t *testing.T) {
	frameTemplate := common.NewFrameTemplate()
	compiler := Compiler{Expander: expandNever}
	template := test.Syntax("id")
	expression, err := ExpressionCompile(compiler, common.NewSyntax(SyntaxForm{template}), &frameTemplate)
	require.NoError(t, err)
	require.Equal(t, SyntaxTemplate{template.Datum(), nil}, expression)
	require.Equal(t, 0, frameTemplate.Size())
}

func TestExpressionCompile_SyntaxPair(t *testing.T) {
	frameTemplate := common.NewFrameTemplate()
	compiler := Compiler{Expander: expandNever}
	template := test.Syntax("(id . thing)")
	expression, err := ExpressionCompile(compiler, common.NewSyntax(SyntaxForm{template}), &frameTemplate)
	require.NoError(t, err)
	require.Equal(t, SyntaxTemplate{template.Datum(), nil}, expression)
	require.Equal(t, 0, frameTemplate.Size())
}

func TestExpressionCompile_SyntaxPatternVariableReference(t *testing.T) {
	frameTemplate := common.NewFrameTemplate()
	compiler := Compiler{Expander: expandNever}
	patternVariable := &common.PatternVariable{Nesting: 0, MatchFrameIndex: 0}
	template := test.Syntax("id")
	template = test.WithBinding(common.NewIdentifier("id"), patternVariable, template)
	expression, err := ExpressionCompile(compiler, common.NewSyntax(SyntaxForm{template}), &frameTemplate)
	require.NoError(t, err)
	syntaxTemplate := expression.(SyntaxTemplate)
	require.Equal(t, PatternVariableReference{patternVariable, common.StackFrameReference{0, 0}}, syntaxTemplate.Template)
	require.Equal(t, 1, len(syntaxTemplate.PatternVariables))
	require.Equal(t, patternVariable, syntaxTemplate.PatternVariables[0])
	require.Equal(t, 0, frameTemplate.Size())
}

func TestExpressionCompile_SyntaxPatternVariableUnexpected(t *testing.T) {
	frameTemplate := common.NewFrameTemplate()
	compiler := Compiler{Expander: expandNever}
	patternVariable := &common.PatternVariable{Nesting: 1}
	template := test.Syntax("id")
	template = test.WithBinding(common.NewIdentifier("id"), patternVariable, template)
	_, err := ExpressionCompile(compiler, common.NewSyntax(SyntaxForm{template}), &frameTemplate)
	require.EqualError(t, err, "compile: in syntax template at (unknown): encountered unexpanded pattern variable")
}

func TestExpressionCompile_SyntaxImproperEllipsis(t *testing.T) {
	frameTemplate := common.NewFrameTemplate()
	compiler := Compiler{Expander: expandNever}
	template := test.Syntax("...")
	template = test.WithBinding(common.NewIdentifier("..."), common.EllipsisKeyword, template)
	_, err := ExpressionCompile(compiler, common.NewSyntax(SyntaxForm{template}), &frameTemplate)
	require.EqualError(t, err, "compile: in syntax template at (unknown): improper use of ellipsis")
}

func TestExpressionCompile_SyntaxPairFirstPatternVariableReference(t *testing.T) {
	frameTemplate := common.NewFrameTemplate()
	compiler := Compiler{Expander: expandNever}
	patternVariable := &common.PatternVariable{Nesting: 0, MatchFrameIndex: 0}
	template := test.Syntax("(id . thing)")
	template = test.WithBinding(common.NewIdentifier("id"), patternVariable, template)
	expression, err := ExpressionCompile(compiler, common.NewSyntax(SyntaxForm{template}), &frameTemplate)
	require.NoError(t, err)
	syntaxTemplate := expression.(SyntaxTemplate)
	pair := syntaxTemplate.Template.(common.Pair)
	require.Equal(t, PatternVariableReference{patternVariable, common.StackFrameReference{0, 0}}, pair.First)
	require.Equal(t, template.PairOrDie().Rest, pair.Rest)
	require.Equal(t, 1, len(syntaxTemplate.PatternVariables))
	require.Equal(t, patternVariable, syntaxTemplate.PatternVariables[0])
	require.Equal(t, 0, frameTemplate.Size())
}

func TestExpressionCompile_SyntaxPairRestPatternVariableReference(t *testing.T) {
	frameTemplate := common.NewFrameTemplate()
	compiler := Compiler{Expander: expandNever}
	patternVariable := &common.PatternVariable{Nesting: 0, MatchFrameIndex: 0}
	template := test.Syntax("(id . thing)")
	template = test.WithBinding(common.NewIdentifier("thing"), patternVariable, template)
	expression, err := ExpressionCompile(compiler, common.NewSyntax(SyntaxForm{template}), &frameTemplate)
	require.NoError(t, err)
	syntaxTemplate := expression.(SyntaxTemplate)
	pair := syntaxTemplate.Template.(common.Pair)
	require.Equal(t, template.PairOrDie().First, pair.First)
	require.Equal(t, PatternVariableReference{patternVariable, common.StackFrameReference{0, 0}}, pair.Rest)
	require.Equal(t, 1, len(syntaxTemplate.PatternVariables))
	require.Equal(t, patternVariable, syntaxTemplate.PatternVariables[0])
	require.Equal(t, 0, frameTemplate.Size())
}

func TestExpressionCompile_SyntaxEllipsis(t *testing.T) {
	frameTemplate := common.NewFrameTemplate()
	compiler := Compiler{Expander: expandNever}
	patternVariable := &common.PatternVariable{Nesting: 1, MatchFrameIndex: 0}
	template := test.Syntax("(id ...)")
	template = test.WithBinding(common.NewIdentifier("id"), patternVariable, template)
	template = test.WithBinding(common.NewIdentifier("..."), common.EllipsisKeyword, template)
	expression, err := ExpressionCompile(compiler, common.NewSyntax(SyntaxForm{template}), &frameTemplate)
	require.NoError(t, err)
	syntaxTemplate := expression.(SyntaxTemplate)
	pair := syntaxTemplate.Template.(common.Pair)
	subtemplate := pair.First.(Subtemplate)
	require.Equal(t, PatternVariableReference{patternVariable, common.StackFrameReference{0, 0}}, subtemplate.Subtemplate.Template)
	require.Equal(t, 1, len(subtemplate.Subtemplate.PatternVariables))
	require.Equal(t, patternVariable, subtemplate.Subtemplate.PatternVariables[0])
	require.Equal(t, 1, subtemplate.Nesting)
	require.Equal(t, 1, len(subtemplate.PatternVariables))
	require.Equal(t, patternVariable, subtemplate.PatternVariables[0])
	require.Equal(t, common.NewSyntax(template.PairOrDie().Rest).PairOrDie().Rest, pair.Rest)
	require.Equal(t, 1, len(syntaxTemplate.PatternVariables))
	require.Equal(t, patternVariable, syntaxTemplate.PatternVariables[0])
	require.Equal(t, 0, frameTemplate.Size())
}

func TestExpressionCompile_SyntaxEllipsisNoPatterVariable(t *testing.T) {
	frameTemplate := common.NewFrameTemplate()
	compiler := Compiler{Expander: expandNever}
	template := test.Syntax("(id ...)")
	template = test.WithBinding(common.NewIdentifier("..."), common.EllipsisKeyword, template)
	_, err := ExpressionCompile(compiler, common.NewSyntax(SyntaxForm{template}), &frameTemplate)
	require.EqualError(t, err, "compile: in syntax template at (unknown): syntax subtemplate must contain a pattern variable")
}

func TestExpressionCompile_SyntaxEllipsisMultiple(t *testing.T) {
	frameTemplate := common.NewFrameTemplate()
	compiler := Compiler{Expander: expandNever}
	patternVariable := &common.PatternVariable{Nesting: 2, MatchFrameIndex: 0}
	template := test.Syntax("(id ... ...)")
	template = test.WithBinding(common.NewIdentifier("id"), patternVariable, template)
	template = test.WithBinding(common.NewIdentifier("..."), common.EllipsisKeyword, template)
	expression, err := ExpressionCompile(compiler, common.NewSyntax(SyntaxForm{template}), &frameTemplate)
	require.NoError(t, err)
	syntaxTemplate := expression.(SyntaxTemplate)
	pair := syntaxTemplate.Template.(common.Pair)
	subtemplate := pair.First.(Subtemplate)
	require.Equal(t, PatternVariableReference{patternVariable, common.StackFrameReference{0, 0}}, subtemplate.Subtemplate.Template)
	require.Equal(t, 1, len(subtemplate.Subtemplate.PatternVariables))
	require.Equal(t, patternVariable, subtemplate.Subtemplate.PatternVariables[0])
	require.Equal(t, 2, subtemplate.Nesting)
	require.Equal(t, 1, len(subtemplate.PatternVariables))
	require.Equal(t, patternVariable, subtemplate.PatternVariables[0])
	require.Equal(t, common.NewSyntax(common.NewSyntax(template.PairOrDie().Rest).PairOrDie().Rest).PairOrDie().Rest, pair.Rest)
	require.Equal(t, 1, len(syntaxTemplate.PatternVariables))
	require.Equal(t, patternVariable, syntaxTemplate.PatternVariables[0])
	require.Equal(t, 0, frameTemplate.Size())
}

func TestExpressionCompile_SyntaxEllipsisInexact(t *testing.T) {
	frameTemplate := common.NewFrameTemplate()
	compiler := Compiler{Expander: expandNever}
	patternVariable := &common.PatternVariable{Nesting: 1}
	template := test.Syntax("((id ...) ...)")
	template = test.WithBinding(common.NewIdentifier("id"), patternVariable, template)
	template = test.WithBinding(common.NewIdentifier("..."), common.EllipsisKeyword, template)
	_, err := ExpressionCompile(compiler, common.NewSyntax(SyntaxForm{template}), &frameTemplate)
	require.EqualError(t, err, "compile: in syntax template at (unknown): syntax subtemplate must contain a pattern variable determining expansion count")
}

func TestExpressionCompile_SyntaxEllipsisInexactMultiple(t *testing.T) {
	frameTemplate := common.NewFrameTemplate()
	compiler := Compiler{Expander: expandNever}
	patternVariable := &common.PatternVariable{Nesting: 1}
	template := test.Syntax("(id ... ...)")
	template = test.WithBinding(common.NewIdentifier("id"), patternVariable, template)
	template = test.WithBinding(common.NewIdentifier("..."), common.EllipsisKeyword, template)
	_, err := ExpressionCompile(compiler, common.NewSyntax(SyntaxForm{template}), &frameTemplate)
	require.EqualError(t, err, "compile: in syntax template at (unknown): syntax subtemplate must contain a pattern variable determining expansion count")
}

func TestExpressionCompile_SyntaxEllipsisIncompatible(t *testing.T) {
	frameTemplate := common.NewFrameTemplate()
	compiler := Compiler{Expander: expandNever}
	patternVariable := &common.PatternVariable{Nesting: 1}
	template := test.Syntax("((id ...) . id)")
	template = test.WithBinding(common.NewIdentifier("id"), patternVariable, template)
	template = test.WithBinding(common.NewIdentifier("..."), common.EllipsisKeyword, template)
	_, err := ExpressionCompile(compiler, common.NewSyntax(SyntaxForm{template}), &frameTemplate)
	require.EqualError(t, err, "compile: in syntax template at (unknown): incompatible expansion counts in first and rest of pair")
}

func TestExpressionCompile_Begin(t *testing.T) {
	frameTemplate := common.NewFrameTemplate()
	form := common.NewSyntax(BeginForm{[]common.Syntax{}})
	expected := test.NewVoidExpression()
	compiler := Compiler{Expander: expandNever, BodyCompiler: func(_ Compiler, _ []common.Syntax, _ common.Scope, ft *common.FrameTemplate) (common.Expression, error) {
		require.True(t, ft == &frameTemplate)
		return expected, nil
	}}
	expression, err := ExpressionCompile(compiler, form, &frameTemplate)
	require.NoError(t, err)
	require.Equal(t, expected, expression)
	require.Equal(t, 0, frameTemplate.Size())
}

func TestExpressionCompile_If(t *testing.T) {
	frameTemplate := common.NewFrameTemplate()
	compiler := Compiler{Expander: expandNever, ExpressionCompiler: expressionCompileIdentity}
	conditionExpression := test.NewVoidExpression()
	thenExpression := test.NewVoidExpression()
	elseExpression := test.NewVoidExpression()
	form := common.NewSyntax(IfForm{common.NewSyntax(conditionExpression), common.NewSyntax(thenExpression), common.NewSyntax(elseExpression)})
	expression, err := ExpressionCompile(compiler, form, &frameTemplate)
	require.NoError(t, err)
	ifExpression := expression.(If)
	require.Equal(t, conditionExpression, ifExpression.Condition)
	require.Equal(t, thenExpression, ifExpression.Then)
	require.Equal(t, elseExpression, ifExpression.Else)
	require.Equal(t, 0, frameTemplate.Size())
}

func TestExpressionCompile_Let(t *testing.T) {
	frameTemplate := common.NewFrameTemplate()
	id := test.Identifier("id")
	initExpression := test.NewVoidExpression()
	bodyExpression := test.NewVoidExpression()
	var binding common.Binding
	compiler := Compiler{Expander: expandNever, ExpressionCompiler: expressionCompileIdentity, BodyCompiler: func(_ Compiler, forms []common.Syntax, _ common.Scope, ft *common.FrameTemplate) (common.Expression, error) {
		require.True(t, ft == &frameTemplate)
		binding = forms[0].IdentifierOrDie().Binding()
		return bodyExpression, nil
	}}
	form := common.NewSyntax(LetForm{id, common.NewSyntax(initExpression), []common.Syntax{test.Syntax("id")}})
	expression, err := ExpressionCompile(compiler, form, &frameTemplate)
	require.NoError(t, err)
	letExpression := expression.(Let)
	require.True(t, binding.(*common.Variable) == letExpression.Variable)
	require.Equal(t, initExpression, letExpression.Init)
	require.Equal(t, bodyExpression, letExpression.Body)
	require.Equal(t, 1, frameTemplate.Size())
}

func TestExpressionCompile_Application(t *testing.T) {
	frameTemplate := common.NewFrameTemplate()
	compiler := Compiler{Expander: expandNever, ExpressionCompiler: expressionCompileIdentity}
	procedureExpression := test.NewVoidExpression()
	argument0Expression := test.NewVoidExpression()
	argument1Expression := test.NewVoidExpression()
	form := common.NewSyntax(ApplicationForm{common.NewSyntax(procedureExpression), []common.Syntax{common.NewSyntax(argument0Expression), common.NewSyntax(argument1Expression)}})
	expression, err := ExpressionCompile(compiler, form, &frameTemplate)
	require.NoError(t, err)
	applicationExpression := expression.(Application)
	require.Equal(t, procedureExpression, applicationExpression.Procedure)
	require.Equal(t, 2, len(applicationExpression.Arguments))
	require.Equal(t, argument0Expression, applicationExpression.Arguments[0])
	require.Equal(t, argument1Expression, applicationExpression.Arguments[1])
	require.Equal(t, 0, frameTemplate.Size())
}

func TestExpressionCompile_Lambda(t *testing.T) {
	frameTemplate := common.NewFrameTemplate()
	id0 := test.Identifier("arg0")
	id1 := test.Identifier("arg1")
	bodyExpression := test.NewVoidExpression()
	var binding0 common.Binding
	var binding1 common.Binding
	compiler := Compiler{Expander: expandNever, BodyCompiler: func(_ Compiler, forms []common.Syntax, _ common.Scope, ft *common.FrameTemplate) (common.Expression, error) {
		require.Equal(t, 2, ft.Size())
		binding0 = common.NewSyntax(forms[0].PairOrDie().First).IdentifierOrDie().Binding()
		binding1 = common.NewSyntax(forms[0].PairOrDie().Rest).IdentifierOrDie().Binding()
		return bodyExpression, nil
	}}
	form := common.NewSyntax(LambdaForm{[]common.Identifier{id0, id1}, []common.Syntax{common.NewSyntax(common.Pair{id0.WrappedSyntax, id1.WrappedSyntax})}})
	expression, err := ExpressionCompile(compiler, form, &frameTemplate)
	require.NoError(t, err)
	lambdaExpression := expression.(Lambda)
	require.Equal(t, 2, lambdaExpression.FrameTemplate.Size())
	require.Equal(t, 2, len(lambdaExpression.Variables))
	require.True(t, binding0.(*common.Variable) == lambdaExpression.Variables[0])
	require.True(t, binding1.(*common.Variable) == lambdaExpression.Variables[1])
	require.Equal(t, 2, len(lambdaExpression.VariableReferences))
	require.Equal(t, lambdaExpression.VariableReferences[0], common.StackFrameReference{0, 0})
	require.Equal(t, lambdaExpression.VariableReferences[1], common.StackFrameReference{0, 1})
	require.Equal(t, bodyExpression, lambdaExpression.Body)
	require.Equal(t, 0, frameTemplate.Size())
}

func TestExpressionCompile_LambdaCapture(t *testing.T) {
	frameTemplate := common.NewFrameTemplate()
	variable := &common.Variable{}
	bodyExpression := test.NewVoidExpression()
	compiler := Compiler{Expander: expandNever, BodyCompiler: func(_ Compiler, forms []common.Syntax, _ common.Scope, ft *common.FrameTemplate) (common.Expression, error) {
		require.Equal(t, 0, ft.Size())
		bindingStackContext := forms[0].IdentifierOrDie().BindingStackContext()
		require.True(t, bindingStackContext.Binding.(*common.Variable) == variable)
		require.Equal(t, common.StackContext(1), bindingStackContext.StackContext)
		return bodyExpression, nil
	}}
	form := common.NewSyntax(LambdaForm{[]common.Identifier{}, []common.Syntax{test.WithBinding(test.Identifier("id"), variable, test.Syntax("id"))}})
	expression, err := ExpressionCompile(compiler, form, &frameTemplate)
	require.NoError(t, err)
	lambdaExpression := expression.(Lambda)
	require.Equal(t, 0, lambdaExpression.FrameTemplate.Size())
	require.Equal(t, 0, len(lambdaExpression.Variables))
	require.Equal(t, 0, len(lambdaExpression.VariableReferences))
	require.Equal(t, bodyExpression, lambdaExpression.Body)
	require.Equal(t, 0, frameTemplate.Size())
}

func TestExpressionCompile_Reference(t *testing.T) {
	frameTemplate := common.NewFrameTemplate()
	compiler := Compiler{Expander: expandNever}
	variable := &common.Variable{}
	id := test.WithBinding(test.Identifier("id"), variable, test.Syntax("id")).IdentifierOrDie()
	form := common.NewSyntax(ReferenceForm{id})
	expression, err := ExpressionCompile(compiler, form, &frameTemplate)
	require.NoError(t, err)
	reference := expression.(Reference)
	require.True(t, variable == reference.Variable)
	require.Equal(t, 0, frameTemplate.Size())
}

func TestExpressionCompile_ReferenceUnbound(t *testing.T) {
	frameTemplate := common.NewFrameTemplate()
	compiler := Compiler{Expander: expandNever}
	id := test.Identifier("id")
	form := common.NewSyntax(ReferenceForm{id})
	_, err := ExpressionCompile(compiler, form, &frameTemplate)
	require.EqualError(t, err, "compile: unbound identifier id at (unknown)")
}

func TestExpressionCompile_ReferenceNonVariable(t *testing.T) {
	frameTemplate := common.NewFrameTemplate()
	compiler := Compiler{Expander: expandNever}
	binding := &common.Keyword{}
	id := test.WithBinding(test.Identifier("id"), binding, test.Syntax("id")).IdentifierOrDie()
	form := common.NewSyntax(ReferenceForm{id})
	_, err := ExpressionCompile(compiler, form, &frameTemplate)
	require.EqualError(t, err, "compile: non-variable identifier id in expression context")
}

func TestExpressionCompile_Set(t *testing.T) {
	frameTemplate := common.NewFrameTemplate()
	compiler := Compiler{Expander: expandNever, ExpressionCompiler: expressionCompileIdentity}
	variable := &common.Variable{}
	id := test.WithBinding(test.Identifier("id"), variable, test.Syntax("id")).IdentifierOrDie()
	valueExpression := test.NewVoidExpression()
	form := common.NewSyntax(SetForm{id, common.NewSyntax(valueExpression)})
	expression, err := ExpressionCompile(compiler, form, &frameTemplate)
	require.NoError(t, err)
	reference := expression.(Set)
	require.True(t, variable == reference.Variable)
	require.Equal(t, valueExpression, reference.Expression)
	require.Equal(t, 0, frameTemplate.Size())
}

func TestExpressionCompile_SetUnbound(t *testing.T) {
	frameTemplate := common.NewFrameTemplate()
	compiler := Compiler{Expander: expandNever, ExpressionCompiler: expressionCompileIdentity}
	id := test.Identifier("id")
	valueExpression := test.NewVoidExpression()
	form := common.NewSyntax(SetForm{id, common.NewSyntax(valueExpression)})
	_, err := ExpressionCompile(compiler, form, &frameTemplate)
	require.EqualError(t, err, "compile: unbound identifier id at (unknown)")
}

func TestExpressionCompile_SetNonVariable(t *testing.T) {
	frameTemplate := common.NewFrameTemplate()
	compiler := Compiler{Expander: expandNever, ExpressionCompiler: expressionCompileIdentity}
	binding := &common.Keyword{}
	id := test.WithBinding(test.Identifier("id"), binding, test.Syntax("id")).IdentifierOrDie()
	valueExpression := test.NewVoidExpression()
	form := common.NewSyntax(SetForm{id, common.NewSyntax(valueExpression)})
	_, err := ExpressionCompile(compiler, form, &frameTemplate)
	require.EqualError(t, err, "compile: non-variable identifier id in assignment")
}

func TestExpressionCompile_SyntaxCase(t *testing.T) {
	t.SkipNow() // TODO:
}

func TestExpressionCompile_LiteralBoolean(t *testing.T) {
	frameTemplate := common.NewFrameTemplate()
	compiler := Compiler{Expander: expandNever}
	form := test.Syntax("#f")
	expression, err := ExpressionCompile(compiler, form, &frameTemplate)
	require.NoError(t, err)
	require.Equal(t, read.MustReadDatum("#f"), expression.(Literal).Datum)
	require.Equal(t, 0, frameTemplate.Size())
}

func TestExpressionCompile_LiteralNumber(t *testing.T) {
	frameTemplate := common.NewFrameTemplate()
	compiler := Compiler{Expander: expandNever}
	form := test.Syntax("123")
	expression, err := ExpressionCompile(compiler, form, &frameTemplate)
	require.NoError(t, err)
	require.Equal(t, read.MustReadDatum("123"), expression.(Literal).Datum)
	require.Equal(t, 0, frameTemplate.Size())
}

func TestExpressionCompile_LiteralCharacter(t *testing.T) {
	frameTemplate := common.NewFrameTemplate()
	compiler := Compiler{Expander: expandNever}
	form := test.Syntax(`#\x`)
	expression, err := ExpressionCompile(compiler, form, &frameTemplate)
	require.NoError(t, err)
	require.Equal(t, read.MustReadDatum(`#\x`), expression.(Literal).Datum)
	require.Equal(t, 0, frameTemplate.Size())
}

func TestExpressionCompile_LiteralString(t *testing.T) {
	frameTemplate := common.NewFrameTemplate()
	compiler := Compiler{Expander: expandNever}
	form := test.Syntax(`"thing"`)
	expression, err := ExpressionCompile(compiler, form, &frameTemplate)
	require.NoError(t, err)
	require.Equal(t, read.MustReadDatum(`"thing"`), expression.(Literal).Datum)
	require.Equal(t, 0, frameTemplate.Size())
}

func TestExpressionCompile_LiteralNull(t *testing.T) {
	frameTemplate := common.NewFrameTemplate()
	compiler := Compiler{Expander: expandNever}
	form := test.Syntax("()")
	expression, err := ExpressionCompile(compiler, form, &frameTemplate)
	require.NoError(t, err)
	require.Equal(t, read.MustReadDatum(`()`), expression.(Literal).Datum)
	require.Equal(t, 0, frameTemplate.Size())
}

func TestExpressionCompile_LiteralVoid(t *testing.T) {
	frameTemplate := common.NewFrameTemplate()
	compiler := Compiler{Expander: expandNever}
	form := common.NewSyntax(common.NewWrappedSyntax(common.Void, nil))
	expression, err := ExpressionCompile(compiler, form, &frameTemplate)
	require.NoError(t, err)
	require.Equal(t, common.Void, expression.(Literal).Datum)
	require.Equal(t, 0, frameTemplate.Size())
}
