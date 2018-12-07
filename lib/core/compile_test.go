package core_test

import (
	"fmt"
	"reflect"
	"testing"

	"github.com/katsuya94/grime/common"
	. "github.com/katsuya94/grime/lib/core"
	"github.com/katsuya94/grime/read"
	"github.com/katsuya94/grime/util"
)

func expandNever(Compiler, common.Datum) (common.Datum, bool, error) {
	return nil, false, nil
}

func expressionCompileIdentity(_ Compiler, form common.Datum) (common.Expression, error) {
	return form.(common.Expression), nil
}

// comparableType must be of non-zero size for equality comparison
type comparableType struct {
	self *comparableType
}

func (comparableType) Evaluate(c common.Continuation) (common.EvaluationResult, error) {
	return common.CallC(c, common.Void)
}

func comparable() common.Expression {
	expression := &comparableType{}
	expression.self = expression
	return expression
}

func data(source string) []common.Datum {
	return read.MustReadString(source)
}

func datum(source string) common.Datum {
	data := data(source)
	if len(data) != 1 {
		panic(fmt.Sprintf("read %v data", len(data)))
	}
	return data[0]
}

func wrap(datum common.Datum) common.WrappedSyntax {
	return common.NewWrappedSyntax(datum)
}

func assertNoError(t *testing.T, err error) {
	if err != nil {
		t.Fatal(err)
	}
}

func assertDeepEquals(t *testing.T, actual, expected interface{}) {
	if !reflect.DeepEqual(actual, expected) {
		t.Fatalf("\nexpected: %#v\n     got: %#v\n", expected, actual)
	}
}

func assertEquals(t *testing.T, actual, expected interface{}) {
	if actual != expected {
		t.Fatalf("\nexpected: %#v\n     got: %#v\n", expected, actual)
	}
}

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
	assertDeepEquals(t, pair.Rest, template.PushOnto(datum("thing")))
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
	assertDeepEquals(t, pair.First, template.PushOnto(datum("id")))
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
	assertDeepEquals(t, pair.Rest, template.PushOnto(datum("()")))
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
	assertDeepEquals(t, pair.Rest, template.PushOnto(datum("()")))
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
	compiler := Compiler{Expander: expandNever, BodyCompiler: func(_ Compiler, forms []common.Datum, defined []common.WrappedSyntax) (common.Expression, []common.WrappedSyntax, error) {
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
	compiler := Compiler{Expander: expandNever, ExpressionCompiler: func(compiler Compiler, form common.Datum) (common.Expression, error) {
		if syntax, ok := form.(common.WrappedSyntax); ok && syntax.Datum() == common.Symbol("id") {
			_, location = syntax.IdentifierAt(compiler.Phase)
			return bodyExpression, nil
		}
		return form.(common.Expression), nil
	}}
	expression, err := ExpressionCompile(compiler, LetForm{identifier, initExpression, wrap(datum("id"))})
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
	compiler := Compiler{Expander: expandNever, ExpressionCompiler: func(compiler Compiler, form common.Datum) (common.Expression, error) {
		syntax := form.(common.WrappedSyntax)
		slice, err := util.Slice(syntax.Datum())
		if err != nil {
			panic(err)
		}
		_, location0 = syntax.PushOnto(slice[0]).IdentifierAt(0)
		_, location1 = syntax.PushOnto(slice[1]).IdentifierAt(0)
		return bodyExpression, nil
	}}
	expression, err := ExpressionCompile(compiler, LambdaForm{[]common.WrappedSyntax{identifier0, identifier1}, wrap(datum("(arg0 arg1)"))})
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
	assertEquals(t, err.Error(), "compile: unbound identifier id")
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
	assertEquals(t, err.Error(), "compile: unbound identifier id")
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

func TestCompile(t *testing.T) {
	t.SkipNow() // TODO
	tests := []struct {
		name   string
		source string
		error  string
	}{
		{
			"malformed quote",
			"(quote)",
			"quote: bad syntax",
		},
		{
			"duplicate definitions: define, define",
			"(define foo 'id) (define foo 'thing)",
			"previously defined: foo",
		},
		{
			"duplicate definitions: define, define-syntax",
			"(define foo 'id) (define-syntax foo (lambda (x) #''thing))",
			"previously defined: foo",
		},
		{
			"duplicate definitions: define-syntax, define",
			"(define-syntax foo (lambda (x) #''thing))  (define foo 'thing)",
			"previously defined: foo",
		},
		{
			"duplicate definitions: define-syntax, define-syntax",
			"(define-syntax foo (lambda (x) #''thing)) (define-syntax foo (lambda (x) #''thing))",
			"previously defined: foo",
		},
		{
			"empty begin in definition context",
			"(begin) 'foo",
			"",
		},
		{
			"body forms after expression in begin",
			"(begin 'foo (define x 'bar))",
			"compile: unexpected body form in expression context",
		},
		{
			"empty lambda",
			"(lambda ())",
			"unexpected final form",
		},
		{
			"empty define procedure",
			"(define (id)) 'foo",
			"unexpected final form",
		},
		{
			"lambda does not leak enclosing context",
			"((~let (x 'foo) (lambda () x))) x",
			"compile: unbound identifier x",
		},
		{
			"lambda does not leak arguments",
			"((lambda (x) x) 'foo) x",
			"compile: unbound identifier x",
		},
		{
			"define-syntax",
			"(define-syntax id (lambda (stx) #''foo)) (id)",
			"",
		},
		{
			"allows nested definitions to shadow",
			"(define id 'foo) (~let (name 'bar) (define id 'baz) id)",
			"",
		},
		{
			"cannot expand underscore",
			"_",
			"cannot expand underscore",
		},
		{
			"cannot expand ellipsis",
			"...",
			"cannot expand ellipsis",
		},
		{
			"ellipsis outside pair",
			"(syntax-case #'foo () (_ #'...))",
			"compile: malformed syntax template",
		},
		{
			"ellipsis in first position",
			"(syntax-case #'foo () (_ #'(...)))",
			"compile: malformed syntax template",
		},
		{
			"ellipsis in first position",
			"(syntax-case #'foo () (id #'(id . ...)))",
			"compile: malformed syntax template",
		},
		{
			"not enough ellipsis",
			"(syntax-case #'((foo)) () (((id ...) ...) #'(id ...)))",
			"compile: pattern variable not fully expanded",
		},
		{
			"not enough ellipsis nested",
			"(syntax-case #'((foo)) () (((id ...) ...) #'((id) ...)))",
			"compile: pattern variable not fully expanded",
		},
		{
			"no pattern variable",
			"(syntax-case #'(foo) () ((_ ...) #'(bar ...)))",
			"compile: syntax subtemplate must contain a pattern variable",
		},
		{
			"no pattern variable determining expansion count",
			"(syntax-case #'(foo) () ((id ...) #'(id ... ...)))",
			"compile: syntax subtemplate must contain a pattern variable determining expansion count",
		},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			sourceBody, err := read.ReadString(test.source)
			if err != nil {
				t.Fatal(err)
			}
			body := common.NewWrappedSyntax(sourceBody)
			for phase, locations := range Bindings {
				for name, location := range locations {
					body = body.SetAt(name, phase, location)
				}
			}
			// Make lambda, syntax available at phase 1
			body = body.SetAt(common.Symbol("lambda"), 1, Bindings[0][common.Symbol("lambda")])
			body = body.SetAt(common.Symbol("syntax"), 1, Bindings[0][common.Symbol("syntax")])
			_, _, err = Compile(common.NewWrappedSyntax(body))
			if test.error != "" {
				if err == nil || err.Error() != test.error {
					t.Fatalf("\nexpected error: %v\n     got error: %v\n", test.error, err)
				}
			} else if err != nil {
				t.Fatal(err)
			}
		})
	}
}
