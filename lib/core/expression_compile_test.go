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
	require.Exactly(t, Literal{datum("id")}, expression)
}

func TestExpressionCompile_SyntaxBoolean(t *testing.T) {
	compiler := Compiler{Expander: expandNever}
	template := wrap(datum("#f"))
	expression, err := ExpressionCompile(compiler, SyntaxForm{template})
	require.NoError(t, err)
	require.Exactly(t, SyntaxTemplate{template, nil}, expression)
}

func TestExpressionCompile_SyntaxNumber(t *testing.T) {
	compiler := Compiler{Expander: expandNever}
	template := wrap(datum("123"))
	expression, err := ExpressionCompile(compiler, SyntaxForm{template})
	require.NoError(t, err)
	require.Exactly(t, SyntaxTemplate{template, nil}, expression)
}

func TestExpressionCompile_SyntaxCharacter(t *testing.T) {
	compiler := Compiler{Expander: expandNever}
	template := wrap(datum(`#\x`))
	expression, err := ExpressionCompile(compiler, SyntaxForm{template})
	require.NoError(t, err)
	require.Exactly(t, SyntaxTemplate{template, nil}, expression)
}

func TestExpressionCompile_SyntaxString(t *testing.T) {
	compiler := Compiler{Expander: expandNever}
	template := wrap(datum(`"thing"`))
	expression, err := ExpressionCompile(compiler, SyntaxForm{template})
	require.NoError(t, err)
	require.Exactly(t, SyntaxTemplate{template, nil}, expression)
}

func TestExpressionCompile_SyntaxNull(t *testing.T) {
	compiler := Compiler{Expander: expandNever}
	template := wrap(datum("()"))
	expression, err := ExpressionCompile(compiler, SyntaxForm{template})
	require.NoError(t, err)
	require.Exactly(t, SyntaxTemplate{template, nil}, expression)
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
	require.EqualError(t, err, "compile: in syntax template at (unknown): encountered unexpanded pattern variable")
}

func TestExpressionCompile_SyntaxImproperEllipsis(t *testing.T) {
	compiler := Compiler{Expander: expandNever}
	template := wrap(datum("id"))
	template = set(template, common.Symbol("id"), Bindings[0][common.Symbol("...")]).(common.WrappedSyntax)
	_, err := ExpressionCompile(compiler, SyntaxForm{template})
	require.EqualError(t, err, "compile: in syntax template at (unknown): improper use of ellipsis")
}

func TestExpressionCompile_SyntaxIdentifier(t *testing.T) {
	compiler := Compiler{Expander: expandNever}
	template := wrap(datum("id"))
	expression, err := ExpressionCompile(compiler, SyntaxForm{template})
	require.NoError(t, err)
	require.Exactly(t, SyntaxTemplate{template, nil}, expression)
}

func TestExpressionCompile_SyntaxPair(t *testing.T) {
	compiler := Compiler{Expander: expandNever}
	template := wrap(datum("(id . thing)"))
	expression, err := ExpressionCompile(compiler, SyntaxForm{template})
	require.NoError(t, err)
	require.Exactly(t, SyntaxTemplate{template, nil}, expression)
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
	require.Exactly(t, template.PushOnto(template.Datum().(common.Pair).Rest, nil), pair.Rest)
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
	require.Exactly(t, template.PushOnto(template.Datum().(common.Pair).First, nil), pair.First)
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
	require.Exactly(t, template.PushOnto(template.Datum().(common.Pair).Rest.(common.Pair).Rest, nil), pair.Rest)
	require.Exactly(t, 1, len(syntaxTemplate.PatternVariables))
	require.Exactly(t, patternVariable, syntaxTemplate.PatternVariables[0])
}

func TestExpressionCompile_SyntaxEllipsisNoPatterVariable(t *testing.T) {
	compiler := Compiler{Expander: expandNever}
	template := wrap(datum("(id ...)"))
	template = set(template, common.Symbol("..."), Bindings[0][common.Symbol("...")]).(common.WrappedSyntax)
	_, err := ExpressionCompile(compiler, SyntaxForm{template})
	require.EqualError(t, err, "compile: in syntax template at (unknown): syntax subtemplate must contain a pattern variable")
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
	require.Exactly(t, template.PushOnto(template.Datum().(common.Pair).Rest.(common.Pair).Rest.(common.Pair).Rest, nil), pair.Rest)
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
	require.EqualError(t, err, "compile: in syntax template at (unknown): syntax subtemplate must contain a pattern variable determining expansion count")
}

func TestExpressionCompile_SyntaxEllipsisInexactMultiple(t *testing.T) {
	compiler := Compiler{Expander: expandNever}
	patternVariable := &common.PatternVariable{Nesting: 1}
	template := wrap(datum("(id ... ...)"))
	template = set(template, common.Symbol("..."), Bindings[0][common.Symbol("...")]).(common.WrappedSyntax)
	template = set(template, common.Symbol("id"), patternVariable).(common.WrappedSyntax)
	_, err := ExpressionCompile(compiler, SyntaxForm{template})
	require.EqualError(t, err, "compile: in syntax template at (unknown): syntax subtemplate must contain a pattern variable determining expansion count")
}

func TestExpressionCompile_SyntaxEllipsisIncompatible(t *testing.T) {
	compiler := Compiler{Expander: expandNever}
	patternVariable := &common.PatternVariable{Nesting: 1}
	template := wrap(datum("((id ...) . id)"))
	template = set(template, common.Symbol("..."), Bindings[0][common.Symbol("...")]).(common.WrappedSyntax)
	template = set(template, common.Symbol("id"), patternVariable).(common.WrappedSyntax)
	_, err := ExpressionCompile(compiler, SyntaxForm{template})
	require.EqualError(t, err, "compile: in syntax template at (unknown): incompatible expansion counts in first and rest of pair")
}

func TestExpressionCompile_Begin(t *testing.T) {
	form := BeginForm{[]common.Datum{}}
	expected := comparable()
	compiler := Compiler{Expander: expandNever, BodyCompiler: func(compiler Compiler, forms []common.Datum, scope common.Scope) (common.Expression, error) {
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
	require.NoError(t, err)
	ifExpression := expression.(If)
	require.Exactly(t, conditionExpression, ifExpression.Condition)
	require.Exactly(t, thenExpression, ifExpression.Then)
	require.Exactly(t, elseExpression, ifExpression.Else)
}

func TestExpressionCompile_Let(t *testing.T) {
	id, _ := wrap(datum("id")).Identifier()
	initExpression := comparable()
	bodyExpression := comparable()
	var location common.Location
	compiler := Compiler{Expander: expandNever, ExpressionCompiler: expressionCompileIdentity, BodyCompiler: func(compiler Compiler, forms []common.Datum, scope common.Scope) (common.Expression, error) {
		id, _ := forms[0].(common.WrappedSyntax).Identifier()
		location = id.Location()
		return bodyExpression, nil
	}}
	expression, err := ExpressionCompile(compiler, LetForm{id, initExpression, []common.Datum{wrap(datum("id"))}})
	require.NoError(t, err)
	letExpression := expression.(Let)
	require.Exactly(t, location, letExpression.Variable)
	require.Exactly(t, initExpression, letExpression.Init)
	require.Exactly(t, bodyExpression, letExpression.Body)
}

func TestExpressionCompile_Application(t *testing.T) {
	compiler := Compiler{Expander: expandNever, ExpressionCompiler: expressionCompileIdentity}
	procedureExpression := comparable()
	argument0Expression := comparable()
	argument1Expression := comparable()
	expression, err := ExpressionCompile(compiler, ApplicationForm{procedureExpression, []common.Datum{argument0Expression, argument1Expression}})
	require.NoError(t, err)
	applicationExpression := expression.(Application)
	require.Exactly(t, procedureExpression, applicationExpression.Procedure, procedureExpression)
	require.Exactly(t, 2, len(applicationExpression.Arguments))
	require.Exactly(t, argument0Expression, applicationExpression.Arguments[0])
	require.Exactly(t, argument1Expression, applicationExpression.Arguments[1])
}

func TestExpressionCompile_Lambda(t *testing.T) {
	id0, _ := wrap(datum("arg0")).Identifier()
	id1, _ := wrap(datum("arg1")).Identifier()
	bodyExpression := comparable()
	var location0 common.Location
	var location1 common.Location
	compiler := Compiler{Expander: expandNever, BodyCompiler: func(compiler Compiler, forms []common.Datum, scope common.Scope) (common.Expression, error) {
		id0, _ := forms[0].(common.Pair).First.(common.WrappedSyntax).Identifier()
		id1, _ := forms[0].(common.Pair).Rest.(common.WrappedSyntax).Identifier()
		location0 = id0.Location()
		location1 = id1.Location()
		return bodyExpression, nil
	}}
	expression, err := ExpressionCompile(compiler, LambdaForm{[]common.Identifier{id0, id1}, []common.Datum{common.Pair{id0.WrappedSyntax, id1.WrappedSyntax}}})
	require.NoError(t, err)
	literalExpression := expression.(Literal)
	lambda := literalExpression.Datum.(common.Lambda)
	require.Exactly(t, 2, len(lambda.Variables))
	require.Exactly(t, location0, lambda.Variables[0])
	require.Exactly(t, location1, lambda.Variables[1])
	require.Exactly(t, bodyExpression, lambda.Body)
}

func TestExpressionCompile_Reference(t *testing.T) {
	compiler := Compiler{Expander: expandNever}
	variable := &common.Variable{}
	id, _ := set(wrap(datum("id")), common.Symbol("id"), variable).(common.WrappedSyntax).Identifier()
	expression, err := ExpressionCompile(compiler, ReferenceForm{id})
	require.NoError(t, err)
	reference := expression.(Reference)
	require.Exactly(t, variable, reference.Variable)
}

func TestExpressionCompile_ReferenceUnbound(t *testing.T) {
	compiler := Compiler{Expander: expandNever}
	id, _ := wrap(datum("id")).Identifier()
	_, err := ExpressionCompile(compiler, ReferenceForm{id})
	require.EqualError(t, err, "compile: unbound identifier id at (unknown)")
}

func TestExpressionCompile_ReferenceNonVariable(t *testing.T) {
	compiler := Compiler{Expander: expandNever}
	location := &common.Keyword{}
	id, _ := set(wrap(datum("id")), common.Symbol("id"), location).(common.WrappedSyntax).Identifier()
	_, err := ExpressionCompile(compiler, ReferenceForm{id})
	require.EqualError(t, err, "compile: non-variable identifier id in expression context")
}

func TestExpressionCompile_Set(t *testing.T) {
	compiler := Compiler{Expander: expandNever, ExpressionCompiler: expressionCompileIdentity}
	variable := &common.Variable{}
	id, _ := set(wrap(datum("id")), common.Symbol("id"), variable).(common.WrappedSyntax).Identifier()
	valueExpression := comparable()
	expression, err := ExpressionCompile(compiler, SetForm{id, valueExpression})
	require.NoError(t, err)
	reference := expression.(Set)
	require.Exactly(t, variable, reference.Variable)
	require.Exactly(t, valueExpression, reference.Expression)
}

func TestExpressionCompile_SetUnbound(t *testing.T) {
	compiler := Compiler{Expander: expandNever, ExpressionCompiler: expressionCompileIdentity}
	id, _ := wrap(datum("id")).Identifier()
	valueExpression := comparable()
	_, err := ExpressionCompile(compiler, SetForm{id, valueExpression})
	require.EqualError(t, err, "compile: unbound identifier id at (unknown)")
}

func TestExpressionCompile_SetNonVariable(t *testing.T) {
	compiler := Compiler{Expander: expandNever, ExpressionCompiler: expressionCompileIdentity}
	location := &common.Keyword{}
	id, _ := set(wrap(datum("id")), common.Symbol("id"), location).(common.WrappedSyntax).Identifier()
	valueExpression := comparable()
	_, err := ExpressionCompile(compiler, SetForm{id, valueExpression})
	require.EqualError(t, err, "compile: non-variable identifier id in assignment")
}

func TestExpressionCompile_SyntaxCase(t *testing.T) {
	t.SkipNow() //TODO
}

func TestExpressionCompile_LiteralBoolean(t *testing.T) {
	compiler := Compiler{Expander: expandNever}
	expression, err := ExpressionCompile(compiler, wrap(datum("#f")))
	require.NoError(t, err)
	require.Exactly(t, datum("#f"), expression.(Literal).Datum)
}

func TestExpressionCompile_LiteralNumber(t *testing.T) {
	compiler := Compiler{Expander: expandNever}
	expression, err := ExpressionCompile(compiler, wrap(datum("123")))
	require.NoError(t, err)
	require.Exactly(t, datum("123"), expression.(Literal).Datum)
}

func TestExpressionCompile_LiteralCharacter(t *testing.T) {
	compiler := Compiler{Expander: expandNever}
	expression, err := ExpressionCompile(compiler, wrap(datum(`#\x`)))
	require.NoError(t, err)
	require.Exactly(t, datum(`#\x`), expression.(Literal).Datum)
}

func TestExpressionCompile_LiteralString(t *testing.T) {
	compiler := Compiler{Expander: expandNever}
	expression, err := ExpressionCompile(compiler, wrap(datum(`"thing"`)))
	require.NoError(t, err)
	require.Exactly(t, datum(`"thing"`), expression.(Literal).Datum)
}

func TestExpressionCompile_LiteralNull(t *testing.T) {
	compiler := Compiler{Expander: expandNever}
	expression, err := ExpressionCompile(compiler, wrap(datum("()")))
	require.NoError(t, err)
	require.Exactly(t, datum("()"), expression.(Literal).Datum)
}

func TestExpressionCompile_LiteralVoid(t *testing.T) {
	compiler := Compiler{Expander: expandNever}
	expression, err := ExpressionCompile(compiler, wrap(common.Void))
	require.NoError(t, err)
	require.Exactly(t, common.Void, expression.(Literal).Datum)
}
