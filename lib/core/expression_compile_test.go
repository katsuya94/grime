package core_test

import (
	"testing"

	"github.com/katsuya94/grime/common"
	. "github.com/katsuya94/grime/lib/core"
)

func TestExpressionCompile_Quote(t *testing.T) {
	compiler := Compiler{Expander: expandNever}
	expression, err := ExpressionCompile(compiler, QuoteForm{datum("id")})
	assertNoError(t, err)
	assertDeepEquals(t, expression, Literal{datum("id")})
}

func TestExpressionCompile_SyntaxBoolean(t *testing.T) {
	compiler := Compiler{Expander: expandNever}
	template := wrap(datum("#f"))
	expression, err := ExpressionCompile(compiler, SyntaxForm{template})
	assertNoError(t, err)
	assertDeepEquals(t, expression, SyntaxTemplate{template, nil})
}

func TestExpressionCompile_SyntaxNumber(t *testing.T) {
	compiler := Compiler{Expander: expandNever}
	template := wrap(datum("123"))
	expression, err := ExpressionCompile(compiler, SyntaxForm{template})
	assertNoError(t, err)
	assertDeepEquals(t, expression, SyntaxTemplate{template, nil})
}

func TestExpressionCompile_SyntaxCharacter(t *testing.T) {
	compiler := Compiler{Expander: expandNever}
	template := wrap(datum(`#\x`))
	expression, err := ExpressionCompile(compiler, SyntaxForm{template})
	assertNoError(t, err)
	assertDeepEquals(t, expression, SyntaxTemplate{template, nil})
}

func TestExpressionCompile_SyntaxString(t *testing.T) {
	compiler := Compiler{Expander: expandNever}
	template := wrap(datum(`"thing"`))
	expression, err := ExpressionCompile(compiler, SyntaxForm{template})
	assertNoError(t, err)
	assertDeepEquals(t, expression, SyntaxTemplate{template, nil})
}

func TestExpressionCompile_SyntaxNull(t *testing.T) {
	compiler := Compiler{Expander: expandNever}
	template := wrap(datum("()"))
	expression, err := ExpressionCompile(compiler, SyntaxForm{template})
	assertNoError(t, err)
	assertDeepEquals(t, expression, SyntaxTemplate{template, nil})
}

func TestExpressionCompile_SyntaxPatternVariableReference(t *testing.T) {
	compiler := Compiler{Expander: expandNever}
	patternVariable := &common.PatternVariable{Nesting: 0}
	template := wrap(datum("id"))
	template = template.Set(common.Symbol("id"), patternVariable)
	expression, err := ExpressionCompile(compiler, SyntaxForm{template})
	assertNoError(t, err)
	syntaxTemplate := expression.(SyntaxTemplate)
	assertEquals(t, syntaxTemplate.Template, PatternVariableReference{patternVariable})
	assertEquals(t, len(syntaxTemplate.PatternVariables), 1)
	assertEquals(t, syntaxTemplate.PatternVariables[0], patternVariable)
}

func TestExpressionCompile_SyntaxPatternVariableUnexpected(t *testing.T) {
	compiler := Compiler{Expander: expandNever}
	patternVariable := &common.PatternVariable{Nesting: 1}
	template := wrap(datum("id"))
	template = template.Set(common.Symbol("id"), patternVariable)
	_, err := ExpressionCompile(compiler, SyntaxForm{template})
	assertEquals(t, err.Error(), "compile: encountered unexpanded pattern variable")
}

func TestExpressionCompile_SyntaxImproperEllipsis(t *testing.T) {
	compiler := Compiler{Expander: expandNever}
	template := wrap(datum("..."))
	template = template.Set(common.Symbol("..."), Bindings[0][common.Symbol("...")])
	_, err := ExpressionCompile(compiler, SyntaxForm{template})
	assertEquals(t, err.Error(), "compile: improper use of ellipsis in syntax template")
}

func TestExpressionCompile_SyntaxIdentifier(t *testing.T) {
	compiler := Compiler{Expander: expandNever}
	template := wrap(datum("id"))
	expression, err := ExpressionCompile(compiler, SyntaxForm{template})
	assertNoError(t, err)
	assertDeepEquals(t, expression, SyntaxTemplate{template, nil})
}

func TestExpressionCompile_SyntaxPair(t *testing.T) {
	compiler := Compiler{Expander: expandNever}
	template := wrap(datum("(id . thing)"))
	expression, err := ExpressionCompile(compiler, SyntaxForm{template})
	assertNoError(t, err)
	assertDeepEquals(t, expression, SyntaxTemplate{template, nil})
}

func TestExpressionCompile_SyntaxPairFirstPatternVariableReference(t *testing.T) {
	compiler := Compiler{Expander: expandNever}
	patternVariable := &common.PatternVariable{Nesting: 0}
	template := wrap(datum("(id . thing)"))
	template = template.Set(common.Symbol("id"), patternVariable)
	expression, err := ExpressionCompile(compiler, SyntaxForm{template})
	assertNoError(t, err)
	syntaxTemplate := expression.(SyntaxTemplate)
	pair := syntaxTemplate.Template.(common.Pair)
	assertEquals(t, pair.First, PatternVariableReference{patternVariable})
	assertDeepEquals(t, pair.Rest, template.PushDown().(common.Pair).Rest)
	assertEquals(t, len(syntaxTemplate.PatternVariables), 1)
	assertEquals(t, syntaxTemplate.PatternVariables[0], patternVariable)
}

func TestExpressionCompile_SyntaxPairRestPatternVariableReference(t *testing.T) {
	compiler := Compiler{Expander: expandNever}
	patternVariable := &common.PatternVariable{Nesting: 0}
	template := wrap(datum("(id . thing)"))
	template = template.Set(common.Symbol("thing"), patternVariable)
	expression, err := ExpressionCompile(compiler, SyntaxForm{template})
	assertNoError(t, err)
	syntaxTemplate := expression.(SyntaxTemplate)
	pair := syntaxTemplate.Template.(common.Pair)
	assertDeepEquals(t, pair.First, template.PushDown().(common.Pair).First)
	assertEquals(t, pair.Rest, PatternVariableReference{patternVariable})
	assertEquals(t, len(syntaxTemplate.PatternVariables), 1)
	assertEquals(t, syntaxTemplate.PatternVariables[0], patternVariable)
}

func TestExpressionCompile_SyntaxEllipsis(t *testing.T) {
	compiler := Compiler{Expander: expandNever}
	patternVariable := &common.PatternVariable{Nesting: 1}
	template := wrap(datum("(id ...)"))
	template = template.Set(common.Symbol("..."), Bindings[0][common.Symbol("...")])
	template = template.Set(common.Symbol("id"), patternVariable)
	expression, err := ExpressionCompile(compiler, SyntaxForm{template})
	assertNoError(t, err)
	syntaxTemplate := expression.(SyntaxTemplate)
	pair := syntaxTemplate.Template.(common.Pair)
	subtemplate := pair.First.(Subtemplate)
	assertEquals(t, subtemplate.Subtemplate.Template, PatternVariableReference{patternVariable})
	assertEquals(t, len(subtemplate.Subtemplate.PatternVariables), 1)
	assertEquals(t, subtemplate.Subtemplate.PatternVariables[0], patternVariable)
	assertEquals(t, subtemplate.Nesting, 1)
	assertEquals(t, len(subtemplate.PatternVariables), 1)
	assertEquals(t, subtemplate.PatternVariables[0], patternVariable)
	assertDeepEquals(t, pair.Rest, template.PushDown().(common.Pair).Rest.(common.WrappedSyntax).PushDown().(common.Pair).Rest)
	assertEquals(t, len(syntaxTemplate.PatternVariables), 1)
	assertEquals(t, syntaxTemplate.PatternVariables[0], patternVariable)
}

func TestExpressionCompile_SyntaxEllipsisNoPatterVariable(t *testing.T) {
	compiler := Compiler{Expander: expandNever}
	template := wrap(datum("(id ...)"))
	template = template.Set(common.Symbol("..."), Bindings[0][common.Symbol("...")])
	_, err := ExpressionCompile(compiler, SyntaxForm{template})
	assertEquals(t, err.Error(), "compile: syntax subtemplate must contain a pattern variable")
}

func TestExpressionCompile_SyntaxEllipsisMultiple(t *testing.T) {
	compiler := Compiler{Expander: expandNever}
	patternVariable := &common.PatternVariable{Nesting: 2}
	template := wrap(datum("(id ... ...)"))
	template = template.Set(common.Symbol("..."), Bindings[0][common.Symbol("...")])
	template = template.Set(common.Symbol("id"), patternVariable)
	expression, err := ExpressionCompile(compiler, SyntaxForm{template})
	assertNoError(t, err)
	syntaxTemplate := expression.(SyntaxTemplate)
	pair := syntaxTemplate.Template.(common.Pair)
	subtemplate := pair.First.(Subtemplate)
	assertEquals(t, subtemplate.Subtemplate.Template, PatternVariableReference{patternVariable})
	assertEquals(t, len(subtemplate.Subtemplate.PatternVariables), 1)
	assertEquals(t, subtemplate.Subtemplate.PatternVariables[0], patternVariable)
	assertEquals(t, subtemplate.Nesting, 2)
	assertEquals(t, len(subtemplate.PatternVariables), 1)
	assertEquals(t, subtemplate.PatternVariables[0], patternVariable)
	assertDeepEquals(t, pair.Rest, template.PushDown().(common.Pair).Rest.(common.WrappedSyntax).PushDown().(common.Pair).Rest.(common.WrappedSyntax).PushDown().(common.Pair).Rest)
	assertEquals(t, len(syntaxTemplate.PatternVariables), 1)
	assertEquals(t, syntaxTemplate.PatternVariables[0], patternVariable)
}

func TestExpressionCompile_SyntaxEllipsisInexact(t *testing.T) {
	compiler := Compiler{Expander: expandNever}
	patternVariable := &common.PatternVariable{Nesting: 1}
	template := wrap(datum("((id ...) ...)"))
	template = template.Set(common.Symbol("..."), Bindings[0][common.Symbol("...")])
	template = template.Set(common.Symbol("id"), patternVariable)
	_, err := ExpressionCompile(compiler, SyntaxForm{template})
	assertEquals(t, err.Error(), "compile: syntax subtemplate must contain a pattern variable determining expansion count")
}

func TestExpressionCompile_SyntaxEllipsisInexactMultiple(t *testing.T) {
	compiler := Compiler{Expander: expandNever}
	patternVariable := &common.PatternVariable{Nesting: 1}
	template := wrap(datum("(id ... ...)"))
	template = template.Set(common.Symbol("..."), Bindings[0][common.Symbol("...")])
	template = template.Set(common.Symbol("id"), patternVariable)
	_, err := ExpressionCompile(compiler, SyntaxForm{template})
	assertEquals(t, err.Error(), "compile: syntax subtemplate must contain a pattern variable determining expansion count")
}

func TestExpressionCompile_SyntaxEllipsisIncompatible(t *testing.T) {
	compiler := Compiler{Expander: expandNever}
	patternVariable := &common.PatternVariable{Nesting: 1}
	template := wrap(datum("((id ...) . id)"))
	template = template.Set(common.Symbol("..."), Bindings[0][common.Symbol("...")])
	template = template.Set(common.Symbol("id"), patternVariable)
	_, err := ExpressionCompile(compiler, SyntaxForm{template})
	assertEquals(t, err.Error(), "compile: incompatible expansion counts for pattern variable")
}

func TestExpressionCompile_Begin(t *testing.T) {
	form := BeginForm{[]common.Datum{comparable()}}
	expected := comparable()
	compiler := Compiler{Expander: expandNever, BodyCompiler: func(compiler Compiler, forms []common.Datum, defined []common.WrappedSyntax) (common.Expression, []common.WrappedSyntax, error) {
		assertEquals(t, compiler.Phase, 0)
		assertDeepEquals(t, forms, form.Forms)
		assertEquals(t, len(defined), 0)
		return expected, nil, nil
	}}
	expression, err := ExpressionCompile(compiler, form)
	assertNoError(t, err)
	assertEquals(t, expression, expected)
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
	identifier := wrap(datum("id"))
	initExpression := comparable()
	bodyExpression := comparable()
	var location common.Location
	compiler := Compiler{Expander: expandNever, ExpressionCompiler: expressionCompileIdentity, BodyCompiler: func(compiler Compiler, forms []common.Datum, defined []common.WrappedSyntax) (common.Expression, []common.WrappedSyntax, error) {
		_, location = forms[0].(common.WrappedSyntax).IdentifierAt(compiler.Phase)
		assertEquals(t, len(defined), 0)
		return bodyExpression, nil, nil
	}}
	expression, err := ExpressionCompile(compiler, LetForm{identifier, initExpression, []common.Datum{wrap(datum("id"))}})
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
	identifier0 := wrap(datum("arg0"))
	identifier1 := wrap(datum("arg1"))
	bodyExpression := comparable()
	var location0 common.Location
	var location1 common.Location
	compiler := Compiler{Expander: expandNever, BodyCompiler: func(compiler Compiler, forms []common.Datum, defined []common.WrappedSyntax) (common.Expression, []common.WrappedSyntax, error) {
		_, location0 = forms[0].(common.WrappedSyntax).IdentifierAt(compiler.Phase)
		_, location1 = forms[1].(common.WrappedSyntax).IdentifierAt(compiler.Phase)
		assertEquals(t, len(defined), 0)
		return bodyExpression, nil, nil
	}}
	expression, err := ExpressionCompile(compiler, LambdaForm{[]common.WrappedSyntax{identifier0, identifier1}, []common.Datum{identifier0, identifier1}})
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
	identifier := wrap(datum("id")).Set(common.Symbol("id"), variable)
	expression, err := ExpressionCompile(compiler, ReferenceForm{identifier})
	assertNoError(t, err)
	reference := expression.(Reference)
	assertEquals(t, reference.Variable, variable)
}

func TestExpressionCompile_ReferenceUnbound(t *testing.T) {
	compiler := Compiler{Expander: expandNever}
	identifier := wrap(datum("id"))
	_, err := ExpressionCompile(compiler, ReferenceForm{identifier})
	assertEquals(t, err.Error(), "compile: unbound identifier id at (unknown)")
}

func TestExpressionCompile_ReferenceNonVariable(t *testing.T) {
	compiler := Compiler{Expander: expandNever}
	location := &common.Keyword{}
	identifier := wrap(datum("id")).Set(common.Symbol("id"), location)
	_, err := ExpressionCompile(compiler, ReferenceForm{identifier})
	assertEquals(t, err.Error(), "compile: non-variable identifier id in expression context")
}

func TestExpressionCompile_Set(t *testing.T) {
	compiler := Compiler{Expander: expandNever, ExpressionCompiler: expressionCompileIdentity}
	variable := &common.Variable{}
	identifier := wrap(datum("id")).Set(common.Symbol("id"), variable)
	valueExpression := comparable()
	expression, err := ExpressionCompile(compiler, SetForm{identifier, valueExpression})
	assertNoError(t, err)
	reference := expression.(Set)
	assertEquals(t, reference.Variable, variable)
	assertEquals(t, reference.Expression, valueExpression)
}

func TestExpressionCompile_SetUnbound(t *testing.T) {
	compiler := Compiler{Expander: expandNever, ExpressionCompiler: expressionCompileIdentity}
	identifier := wrap(datum("id"))
	valueExpression := comparable()
	_, err := ExpressionCompile(compiler, SetForm{identifier, valueExpression})
	assertEquals(t, err.Error(), "compile: unbound identifier id at (unknown)")
}

func TestExpressionCompile_SetNonVariable(t *testing.T) {
	compiler := Compiler{Expander: expandNever, ExpressionCompiler: expressionCompileIdentity}
	location := &common.Keyword{}
	identifier := wrap(datum("id")).Set(common.Symbol("id"), location)
	valueExpression := comparable()
	_, err := ExpressionCompile(compiler, SetForm{identifier, valueExpression})
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
