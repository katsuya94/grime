package core_test

import (
	"reflect"
	"testing"

	"github.com/katsuya94/grime/common"
	. "github.com/katsuya94/grime/lib/core"
	"github.com/katsuya94/grime/read"
)

func TestEvaluateExpression(t *testing.T) {
	tests := []struct {
		name   string
		source string
		val    string
		error  string
	}{
		{
			"boolean literal",
			"#f",
			"#f",
			"",
		},
		{
			"number literal",
			"123",
			"123",
			"",
		},
		{
			"character literal",
			`#\x`,
			`#\x`,
			"",
		},
		{
			"string literal",
			`"name"`,
			`"name"`,
			"",
		},
		{
			"symbol literal",
			"'id",
			"id",
			"",
		},
		{
			"list literal",
			"'(id)",
			"(id)",
			"",
		},
		{
			"if then",
			"(if #t 'foo 'bar)",
			"foo",
			"",
		},
		{
			"if else",
			"(if #f 'foo 'bar)",
			"bar",
			"",
		},
		{
			"if non-boolean",
			"(if 'id 'foo 'bar)",
			"foo",
			"",
		},
		{
			"let",
			"(~let (x 'id) x)",
			"id",
			"",
		},
		{
			"define",
			"(~define x 'foo) x",
			"foo",
			"",
		},
		{
			"begin in splicing context",
			"(begin 'foo 'bar)",
			"bar",
			"",
		},
		{
			"begin in expression context",
			"(cons (begin 'foo 'bar) '())",
			"(bar)",
			"",
		},
		{
			"begin with one subform",
			"(begin 'foo)",
			"foo",
			"",
		},
		{
			"begin with multiple subforms",
			"(begin 'bar 'foo)",
			"foo",
			"",
		},
		{
			"begin with body forms",
			"(begin (~define x 'foo) x)",
			"foo",
			"",
		},
		{
			"lambda",
			"((lambda () 'foo))",
			"foo",
			"",
		},
		{
			"lambda with argument",
			"((lambda (x) x) 'foo)",
			"foo",
			"",
		},
		{
			"lambda using enclosing context",
			"((~let (x 'foo) (lambda () x)))",
			"foo",
			"",
		},
		{
			"call/cc used to escape early",
			"(call/cc (lambda (c) (c 'foo) 'bar))",
			"foo",
			"",
		},
		{
			"set! used to set a defined variable",
			"(~define x 'bar) (set! x 'foo) x",
			"foo",
			"",
		},
		{
			"call/cc and set! used to loop",
			`
			(~define in '(foo bar baz))
			(~define continue #f)
			(~define out (call/cc (lambda (c) (set! continue c) '())))
			(if (null? in)
				out
				(begin
					(~define new (cons (car in) out))
					(set! in (cdr in))
					(continue new)))
			`,
			"(baz bar foo)",
			"",
		},
		{
			"error raises errors",
			`(error "well that's too bad")`,
			"",
			"well that's too bad",
		},
		{
			"cannot reference identifer before its definition",
			"(~define foo (lambda () bar)) (~define baz (foo)) (~define bar 'id) baz",
			"",
			"evaluate: cannot reference identifier before its definition",
		},
		{
			"cannot set identifer before its definition",
			"(~define foo (lambda () (set! bar 'thing))) (~define baz (foo)) (~define bar 'id) baz",
			"",
			"evaluate: cannot set identifier before its definition",
		},
		{
			"define-syntax",
			"(define-syntax id (lambda (stx) #''foo)) (id)",
			"foo",
			"",
		},
		{
			"syntax-case wildcard",
			"(syntax->datum (syntax-case #'bar () (_ #'foo)))",
			"foo",
			"",
		},
		{
			"syntax-case capture",
			"(syntax->datum (syntax-case #'foo () (id #'id)))",
			"foo",
			"",
		},
		{
			"syntax-case ellipsis",
			"(syntax->datum (syntax-case #'(foo bar) () ((id ...) #'(id ...))))",
			"(foo bar)",
			"",
		},
		{
			"syntax-case nested ellipsis",
			"(syntax->datum (syntax-case #'((foo) (bar baz)) () (((id ...) ...) #'((id ...) ...))))",
			"((foo) (bar baz))",
			"",
		},
		{
			"syntax-case nested ellipsis flattened",
			"(syntax->datum (syntax-case #'((foo) (bar baz)) () (((id ...) ...) #'(id ... ...))))",
			"(foo bar baz)",
			"",
		},
		{
			"syntax-case ellipsis repeated",
			"(syntax->datum (syntax-case #'((foo bar) (baz qux)) () (((id ...) (thing ...)) #'((id thing ...) ...))))",
			"((foo baz qux) (bar baz qux))",
			"",
		},
		{
			"syntax-case failure",
			"(syntax-case #'bar () ((id) #'foo))",
			"",
			"bad syntax",
		},
		{
			"syntax-case fender",
			"(syntax-case #'bar () (_ #f #'foo))",
			"",
			"bad syntax",
		},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			var expected common.Datum
			if test.val != "" {
				expected = read.MustReadDatum(test.val)
			}
			syntaxes, nullSourceLocationTree := read.MustReadSyntaxes(test.source)
			body := common.Body(nullSourceLocationTree, syntaxes...)
			scopes := make(map[int]*common.Scope)
			for phase, locations := range Bindings {
				scopes[phase] = common.NewScope(phase)
				for name, location := range locations {
					scopes[phase].Set(common.NewIdentifier(name), location)
				}
			}
			// Make lambda, syntax available at phase 1
			if _, ok := scopes[1]; !ok {
				scopes[1] = common.NewScope(1)
			}
			scopes[1].Set(common.NewIdentifier(common.Symbol("lambda")), Bindings[0][common.Symbol("lambda")])
			scopes[1].Set(common.NewIdentifier(common.Symbol("syntax")), Bindings[0][common.Symbol("syntax")])
			for _, scope := range scopes {
				body = body.Push(scope)
			}
			expression, err := Compile(body, scopes[0])
			if err != nil {
				t.Fatal(err)
			}
			actual, err := common.EvaluateOnce(expression)
			if test.error != "" {
				if err == nil || err.Error() != test.error {
					t.Fatalf("\nexpected error: %v\n     got error: %v\n", test.error, err)
				}
			} else if err != nil {
				t.Fatal(err)
			} else if !reflect.DeepEqual(actual, expected) {
				t.Fatalf("\nexpected: %#v\n     got: %#v", expected, actual)
			}
		})
	}
}
