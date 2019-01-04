package core_test

import (
	"testing"

	"github.com/stretchr/testify/require"

	"github.com/katsuya94/grime/common"
	. "github.com/katsuya94/grime/lib/core"
)

func TestExpressionCompile_Quote(t *testing.T) {
	compiler := Compiler{Expander: expandNever}
	expression, err := ExpressionCompile(compiler, QuoteForm{datum("id")})
	require.NoError(t, err)
	require.Equal(t, Literal{datum("id")}, expression)
}

func TestExpressionCompile_SyntaxBoolean(t *testing.T) {
	compiler := Compiler{Expander: expandNever}
	template := wrap(datum("#f"))
	expression, err := ExpressionCompile(compiler, SyntaxForm{template})
	require.NoError(t, err)
	require.Equal(t, SyntaxTemplate{template, nil}, expression)
}

func TestExpressionCompile_SyntaxNumber(t *testing.T) {
	compiler := Compiler{Expander: expandNever}
	template := wrap(datum("123"))
	expression, err := ExpressionCompile(compiler, SyntaxForm{template})
	require.NoError(t, err)
	require.Equal(t, SyntaxTemplate{template, nil}, expression)
}

func TestExpressionCompile_SyntaxCharacter(t *testing.T) {
	compiler := Compiler{Expander: expandNever}
	template := wrap(datum(`#\x`))
	expression, err := ExpressionCompile(compiler, SyntaxForm{template})
	require.NoError(t, err)
	require.Equal(t, SyntaxTemplate{template, nil}, expression)
}

func TestExpressionCompile_SyntaxString(t *testing.T) {
	compiler := Compiler{Expander: expandNever}
	template := wrap(datum(`"thing"`))
	expression, err := ExpressionCompile(compiler, SyntaxForm{template})
	require.NoError(t, err)
	require.Equal(t, SyntaxTemplate{template, nil}, expression)
}

func TestExpressionCompile_SyntaxNull(t *testing.T) {
	compiler := Compiler{Expander: expandNever}
	template := wrap(datum("()"))
	expression, err := ExpressionCompile(compiler, SyntaxForm{template})
	require.NoError(t, err)
	require.Equal(t, SyntaxTemplate{template, nil}, expression)
}

func TestExpressionCompile_SyntaxPatternVariableReference(t *testing.T) {
	compiler := Compiler{Expander: expandNever}
	patternVariable := &common.PatternVariable{Nesting: 0}
	template := wrap(datum("id"))
	template = set(template, common.Symbol("id"), patternVariable).(common.WrappedSyntax)
	expression, err := ExpressionCompile(compiler, SyntaxForm{template})
	require.NoError(t, err)
	syntaxTemplate := expression.(SyntaxTemplate)
	require.Exactly(t, PatternVariableReference{patternVariable}, syntaxTemplate.Template)
	require.Exactly(t, 1, len(syntaxTemplate.PatternVariables))
	require.Exactly(t, patternVariable, syntaxTemplate.PatternVariables[0])
}

func TestExpressionCompile_SyntaxPatternVariableUnexpected(t *testing.T) {
	compiler := Compiler{Expander: expandNever}
	patternVariable := &common.PatternVariable{Nesting: 1}
	template := wrap(datum("id"))
	template = set(template, common.Symbol("id"), patternVariable).(common.WrappedSyntax)
	_, err := ExpressionCompile(compiler, SyntaxForm{template})
	require.EqualError(t, err, "compile: encountered unexpanded pattern variable")
}

func TestExpressionCompile_SyntaxImproperEllipsis(t *testing.T) {
	compiler := Compiler{Expander: expandNever}
	template := wrap(datum("id"))
	template = set(template, common.Symbol("id"), Bindings[0][common.Symbol("...")]).(common.WrappedSyntax)
	_, err := ExpressionCompile(compiler, SyntaxForm{template})
	require.EqualError(t, err, "compile: improper use of ellipsis in syntax template")
}

func TestExpressionCompile_SyntaxIdentifier(t *testing.T) {
	compiler := Compiler{Expander: expandNever}
	template := wrap(datum("id"))
	expression, err := ExpressionCompile(compiler, SyntaxForm{template})
	require.NoError(t, err)
	require.Equal(t, SyntaxTemplate{template, nil}, expression)
}

func TestExpressionCompile_SyntaxPair(t *testing.T) {
	compiler := Compiler{Expander: expandNever}
	template := wrap(datum("(id . thing)"))
	expression, err := ExpressionCompile(compiler, SyntaxForm{template})
	require.NoError(t, err)
	require.Equal(t, SyntaxTemplate{template, nil}, expression)
}

func TestExpressionCompile_SyntaxPairFirstPatternVariableReference(t *testing.T) {
	compiler := Compiler{Expander: expandNever}
	patternVariable := &common.PatternVariable{Nesting: 0}
	template := wrap(datum("(id . thing)"))
	template = set(template, common.Symbol("id"), patternVariable).(common.WrappedSyntax)
	expression, err := ExpressionCompile(compiler, SyntaxForm{template})
	require.NoError(t, err)
	syntaxTemplate := expression.(SyntaxTemplate)
	pair := syntaxTemplate.Template.(common.Pair)
	require.Exactly(t, PatternVariableReference{patternVariable}, pair.First)
	require.Equal(t, template.PushDown().(common.Pair).Rest, pair.Rest)
	require.Exactly(t, 1, len(syntaxTemplate.PatternVariables))
	require.Exactly(t, patternVariable, syntaxTemplate.PatternVariables[0])
}

func TestExpressionCompile_SyntaxPairRestPatternVariableReference(t *testing.T) {
	compiler := Compiler{Expander: expandNever}
	patternVariable := &common.PatternVariable{Nesting: 0}
	template := wrap(datum("(id . thing)"))
	template = set(template, common.Symbol("thing"), patternVariable).(common.WrappedSyntax)
	expression, err := ExpressionCompile(compiler, SyntaxForm{template})
	require.NoError(t, err)
	syntaxTemplate := expression.(SyntaxTemplate)
	pair := syntaxTemplate.Template.(common.Pair)
	require.Equal(t, template.PushDown().(common.Pair).First, pair.First)
	require.Exactly(t, PatternVariableReference{patternVariable}, pair.Rest)
	require.Exactly(t, 1, len(syntaxTemplate.PatternVariables))
	require.Exactly(t, patternVariable, syntaxTemplate.PatternVariables[0])
}

func TestExpressionCompile_SyntaxEllipsis(t *testing.T) {
	compiler := Compiler{Expander: expandNever}
	patternVariable := &common.PatternVariable{Nesting: 1}
	template := wrap(datum("(id ...)"))
	template = set(template, common.Symbol("..."), Bindings[0][common.Symbol("...")]).(common.WrappedSyntax)
	template = set(template, common.Symbol("id"), patternVariable).(common.WrappedSyntax)
	expression, err := ExpressionCompile(compiler, SyntaxForm{template})
	require.NoError(t, err)
	syntaxTemplate := expression.(SyntaxTemplate)
	pair := syntaxTemplate.Template.(common.Pair)
	subtemplate := pair.First.(Subtemplate)
	require.Exactly(t, PatternVariableReference{patternVariable}, subtemplate.Subtemplate.Template)
	require.Exactly(t, 1, len(subtemplate.Subtemplate.PatternVariables))
	require.Exactly(t, patternVariable, subtemplate.Subtemplate.PatternVariables[0])
	require.Exactly(t, 1, subtemplate.Nesting)
	require.Exactly(t, 1, len(subtemplate.PatternVariables))
	require.Exactly(t, patternVariable, subtemplate.PatternVariables[0])
	require.Equal(t, template.PushDown().(common.Pair).Rest.(common.WrappedSyntax).PushDown().(common.Pair).Rest, pair.Rest)
	require.Exactly(t, 1, len(syntaxTemplate.PatternVariables))
	require.Exactly(t, patternVariable, syntaxTemplate.PatternVariables[0])
}

func TestExpressionCompile_SyntaxEllipsisNoPatterVariable(t *testing.T) {
	compiler := Compiler{Expander: expandNever}
	template := wrap(datum("(id ...)"))
	template = set(template, common.Symbol("..."), Bindings[0][common.Symbol("...")]).(common.WrappedSyntax)
	_, err := ExpressionCompile(compiler, SyntaxForm{template})
	require.EqualError(t, err, "compile: syntax subtemplate must contain a pattern variable")
}

func TestExpressionCompile_SyntaxEllipsisMultiple(t *testing.T) {
	compiler := Compiler{Expander: expandNever}
	patternVariable := &common.PatternVariable{Nesting: 2}
	template := wrap(datum("(id ... ...)"))
	template = set(template, common.Symbol("..."), Bindings[0][common.Symbol("...")]).(common.WrappedSyntax)
	template = set(template, common.Symbol("id"), patternVariable).(common.WrappedSyntax)
	expression, err := ExpressionCompile(compiler, SyntaxForm{template})
	require.NoError(t, err)
	syntaxTemplate := expression.(SyntaxTemplate)
	pair := syntaxTemplate.Template.(common.Pair)
	subtemplate := pair.First.(Subtemplate)
	require.Exactly(t, PatternVariableReference{patternVariable}, subtemplate.Subtemplate.Template)
	require.Exactly(t, 1, len(subtemplate.Subtemplate.PatternVariables))
	require.Exactly(t, patternVariable, subtemplate.Subtemplate.PatternVariables[0])
	require.Exactly(t, 2, subtemplate.Nesting)
	require.Exactly(t, 1, len(subtemplate.PatternVariables))
	require.Exactly(t, patternVariable, subtemplate.PatternVariables[0])
	require.Equal(t, template.PushDown().(common.Pair).Rest.(common.WrappedSyntax).PushDown().(common.Pair).Rest.(common.WrappedSyntax).PushDown().(common.Pair).Rest, pair.Rest)
	require.Exactly(t, 1, len(syntaxTemplate.PatternVariables))
	require.Exactly(t, patternVariable, syntaxTemplate.PatternVariables[0])
}

func TestExpressionCompile_SyntaxEllipsisInexact(t *testing.T) {
	compiler := Compiler{Expander: expandNever}
	patternVariable := &common.PatternVariable{Nesting: 1}
	template := wrap(datum("((id ...) ...)"))
	template = set(template, common.Symbol("..."), Bindings[0][common.Symbol("...")]).(common.WrappedSyntax)
	template = set(template, common.Symbol("id"), patternVariable).(common.WrappedSyntax)
	_, err := ExpressionCompile(compiler, SyntaxForm{template})
	require.EqualError(t, err, "compile: syntax subtemplate must contain a pattern variable determining expansion count")
}

func TestExpressionCompile_SyntaxEllipsisInexactMultiple(t *testing.T) {
	compiler := Compiler{Expander: expandNever}
	patternVariable := &common.PatternVariable{Nesting: 1}
	template := wrap(datum("(id ... ...)"))
	template = set(template, common.Symbol("..."), Bindings[0][common.Symbol("...")]).(common.WrappedSyntax)
	template = set(template, common.Symbol("id"), patternVariable).(common.WrappedSyntax)
	_, err := ExpressionCompile(compiler, SyntaxForm{template})
	require.EqualError(t, err, "compile: syntax subtemplate must contain a pattern variable determining expansion count")
}

func TestExpressionCompile_SyntaxEllipsisIncompatible(t *testing.T) {
	compiler := Compiler{Expander: expandNever}
	patternVariable := &common.PatternVariable{Nesting: 1}
	template := wrap(datum("((id ...) . id)"))
	template = set(template, common.Symbol("..."), Bindings[0][common.Symbol("...")]).(common.WrappedSyntax)
	template = set(template, common.Symbol("id"), patternVariable).(common.WrappedSyntax)
	_, err := ExpressionCompile(compiler, SyntaxForm{template})
	require.EqualError(t, err, "compile: incompatible expansion counts for pattern variable")
}

func TestExpressionCompile_Begin(t *testing.T) {
	form := BeginForm{[]common.Datum{}}
	expected := comparable()
	compiler := Compiler{Expander: expandNever, BodyCompiler: func(compiler Compiler, forms []common.Datum, scope *common.Scope) (common.Expression, error) {
		require.Exactly(t, 0, compiler.Phase)
		return expected, nil
	}}
	expression, err := ExpressionCompile(compiler, form)
	require.NoError(t, err)
	require.Exactly(t, expected, expression)
}

func TestExpressionCompile_If(t *testing.T) {
	compiler := Compiler{Expander: expandNever, ExpressionCompiler: expressionCompileIdentity}
	conditionExpression := comparable()
	thenExpression := comparable()
	elseExpression := comparable()
	expression, err := ExpressionCompile(compiler, IfForm{conditionExpression, thenExpression, elseExpression})
	assertNoError(t, err)
	ifExpression := expression.(If)
	assertEquals(t, ifExpression.Condition, conditionExpression)
	assertEquals(t, ifExpression.Then, thenExpression)
	assertEquals(t, ifExpression.Else, elseExpression)
}

func TestExpressionCompile_Let(t *testing.T) {
	id, _ := wrap(datum("id")).Identifier()
	initExpression := comparable()
	bodyExpression := comparable()
	var location common.Location
	compiler := Compiler{Expander: expandNever, ExpressionCompiler: expressionCompileIdentity, BodyCompiler: func(compiler Compiler, forms []common.Datum, scope *common.Scope) (common.Expression, error) {
		require.Exactly(t, 0, compiler.Phase)
		id, _ := forms[0].(common.WrappedSyntax).Identifier()
		location = id.Location()
		return bodyExpression, nil
	}}
	expression, err := ExpressionCompile(compiler, LetForm{id, initExpression, []common.Datum{wrap(datum("id"))}})
	assertNoError(t, err)
	letExpression := expression.(Let)
	assertEquals(t, letExpression.Variable, location)
	assertEquals(t, letExpression.Init, initExpression)
	assertEquals(t, letExpression.Body, bodyExpression)
}

func TestExpressionCompile_Application(t *testing.T) {
	compiler := Compiler{Expander: expandNever, ExpressionCompiler: expressionCompileIdentity}
	procedureExpression := comparable()
	argument0Expression := comparable()
	argument1Expression := comparable()
	expression, err := ExpressionCompile(compiler, ApplicationForm{procedureExpression, []common.Datum{argument0Expression, argument1Expression}})
	assertNoError(t, err)
	applicationExpression := expression.(Application)
	assertEquals(t, applicationExpression.Procedure, procedureExpression)
	assertEquals(t, len(applicationExpression.Arguments), 2)
	assertEquals(t, applicationExpression.Arguments[0], argument0Expression)
	assertEquals(t, applicationExpression.Arguments[1], argument1Expression)
}

func TestExpressionCompile_Lambda(t *testing.T) {
	id0, _ := wrap(datum("arg0")).Identifier()
	id1, _ := wrap(datum("arg1")).Identifier()
	bodyExpression := comparable()
	var location0 common.Location
	var location1 common.Location
	compiler := Compiler{Expander: expandNever, BodyCompiler: func(compiler Compiler, forms []common.Datum, scope *common.Scope) (common.Expression, error) {
		require.Exactly(t, 0, compiler.Phase)
		id0, _ := forms[0].(common.Pair).First.(common.WrappedSyntax).Identifier()
		id1, _ := forms[0].(common.Pair).Rest.(common.WrappedSyntax).Identifier()
		location0 = id0.Location()
		location1 = id1.Location()
		return bodyExpression, nil
	}}
	expression, err := ExpressionCompile(compiler, LambdaForm{[]common.Identifier{id0, id1}, []common.Datum{common.Pair{id0.WrappedSyntax, id1.WrappedSyntax}}})
	assertNoError(t, err)
	literalExpression := expression.(Literal)
	lambda := literalExpression.Datum.(common.Lambda)
	assertEquals(t, len(lambda.Variables), 2)
	assertEquals(t, lambda.Variables[0], location0)
	assertEquals(t, lambda.Variables[1], location1)
	assertEquals(t, lambda.Body, bodyExpression)
}

func TestExpressionCompile_Reference(t *testing.T) {
	compiler := Compiler{Expander: expandNever}
	variable := &common.Variable{}
	id, _ := set(wrap(datum("id")), common.Symbol("id"), variable).(common.WrappedSyntax).Identifier()
	expression, err := ExpressionCompile(compiler, ReferenceForm{id})
	assertNoError(t, err)
	reference := expression.(Reference)
	assertEquals(t, reference.Variable, variable)
}

func TestExpressionCompile_ReferenceUnbound(t *testing.T) {
	compiler := Compiler{Expander: expandNever}
	id, _ := wrap(datum("id")).Identifier()
	_, err := ExpressionCompile(compiler, ReferenceForm{id})
	assertEquals(t, err.Error(), "compile: unbound identifier id at (unknown)")
}

func TestExpressionCompile_ReferenceNonVariable(t *testing.T) {
	compiler := Compiler{Expander: expandNever}
	location := &common.Keyword{}
	id, _ := set(wrap(datum("id")), common.Symbol("id"), location).(common.WrappedSyntax).Identifier()
	_, err := ExpressionCompile(compiler, ReferenceForm{id})
	assertEquals(t, err.Error(), "compile: non-variable identifier id in expression context")
}

func TestExpressionCompile_Set(t *testing.T) {
	compiler := Compiler{Expander: expandNever, ExpressionCompiler: expressionCompileIdentity}
	variable := &common.Variable{}
	id, _ := set(wrap(datum("id")), common.Symbol("id"), variable).(common.WrappedSyntax).Identifier()
	valueExpression := comparable()
	expression, err := ExpressionCompile(compiler, SetForm{id, valueExpression})
	assertNoError(t, err)
	reference := expression.(Set)
	assertEquals(t, reference.Variable, variable)
	assertEquals(t, reference.Expression, valueExpression)
}

func TestExpressionCompile_SetUnbound(t *testing.T) {
	compiler := Compiler{Expander: expandNever, ExpressionCompiler: expressionCompileIdentity}
	id, _ := wrap(datum("id")).Identifier()
	valueExpression := comparable()
	_, err := ExpressionCompile(compiler, SetForm{id, valueExpression})
	assertEquals(t, err.Error(), "compile: unbound identifier id at (unknown)")
}

func TestExpressionCompile_SetNonVariable(t *testing.T) {
	compiler := Compiler{Expander: expandNever, ExpressionCompiler: expressionCompileIdentity}
	location := &common.Keyword{}
	id, _ := set(wrap(datum("id")), common.Symbol("id"), location).(common.WrappedSyntax).Identifier()
	valueExpression := comparable()
	_, err := ExpressionCompile(compiler, SetForm{id, valueExpression})
	assertEquals(t, err.Error(), "compile: non-variable identifier id in assignment")
}

func TestExpressionCompile_SyntaxCase(t *testing.T) {
	t.SkipNow() //TODO
}

func TestExpressionCompile_LiteralBoolean(t *testing.T) {
	compiler := Compiler{Expander: expandNever}
	expression, err := ExpressionCompile(compiler, wrap(datum("#f")))
	assertNoError(t, err)
	assertEquals(t, expression.(Literal).Datum, datum("#f"))
}

func TestExpressionCompile_LiteralNumber(t *testing.T) {
	compiler := Compiler{Expander: expandNever}
	expression, err := ExpressionCompile(compiler, wrap(datum("123")))
	assertNoError(t, err)
	assertEquals(t, expression.(Literal).Datum, datum("123"))
}

func TestExpressionCompile_LiteralCharacter(t *testing.T) {
	compiler := Compiler{Expander: expandNever}
	expression, err := ExpressionCompile(compiler, wrap(datum(`#\x`)))
	assertNoError(t, err)
	assertEquals(t, expression.(Literal).Datum, datum(`#\x`))
}

func TestExpressionCompile_LiteralString(t *testing.T) {
	compiler := Compiler{Expander: expandNever}
	expression, err := ExpressionCompile(compiler, wrap(datum(`"thing"`)))
	assertNoError(t, err)
	assertEquals(t, expression.(Literal).Datum, datum(`"thing"`))
}

func TestExpressionCompile_LiteralNull(t *testing.T) {
	compiler := Compiler{Expander: expandNever}
	expression, err := ExpressionCompile(compiler, wrap(datum("()")))
	assertNoError(t, err)
	assertEquals(t, expression.(Literal).Datum, datum("()"))
}

func TestExpressionCompile_LiteralVoid(t *testing.T) {
	compiler := Compiler{Expander: expandNever}
	expression, err := ExpressionCompile(compiler, wrap(common.Void))
	assertNoError(t, err)
	assertEquals(t, expression.(Literal).Datum, common.Void)
}
