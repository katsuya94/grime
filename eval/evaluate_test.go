package eval_test

import (
	"reflect"
	"testing"

	"github.com/katsuya94/grime/common"
	. "github.com/katsuya94/grime/eval"
	"github.com/katsuya94/grime/lib/core"
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
			"let*",
			"(let* ((x 'id)) x)",
			"id",
			"",
		},
		{
			"let* multiple",
			"(let* ((x 'id) (y 'name)) (cons x y))",
			"(id . name)",
			"",
		},
		{
			"define",
			"(define x 'foo) x",
			"foo",
			"",
		},
		{
			"define procedure",
			"(define (id) 'foo) (id)",
			"foo",
			"",
		},
		{
			"define procedure with argument",
			"(define (id x) x) (id 'foo)",
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
			"(begin (define x 'foo) x)",
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
			"((let* ((x 'foo)) (lambda () x)))",
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
			"(define x 'bar) (set! x 'foo) x",
			"foo",
			"",
		},
		{
			"call/cc and set! used to loop",
			`
			(define x '(foo bar baz))
			(define y '())
			(define c (call/cc (lambda (c) c)))
			(if (null? x)
				y
				(begin
				(set! y (cons (car x) y))
				(set! x (cdr x))
				(c c)))
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
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			data, err := read.ReadString(test.val)
			if err != nil {
				t.Fatal(err)
			} else if len(data) == 0 {
				data = []common.Datum{nil}
			}
			expected := data[0]
			body, err := read.ReadString(test.source)
			if err != nil {
				t.Fatal(err)
			}
			environment := common.NewEnvironment(core.Bindings)
			form, err := ExpandBody(environment, body)
			if err != nil {
				t.Fatal(err)
			}
			expression, err := Compile(environment, form)
			if err != nil {
				t.Fatal(err)
			}
			actual, err := EvaluateExpressionOnce(expression)
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
