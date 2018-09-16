package eval_test

import (
	"reflect"
	"testing"

	"github.com/katsuya94/grime/common"
	. "github.com/katsuya94/grime/eval"
	"github.com/katsuya94/grime/lib/core"
	"github.com/katsuya94/grime/read"
)

func TestExpandBody(t *testing.T) {
	tests := []struct {
		name     string
		source   string
		expected string
		error    string
	}{
		{
			"malformed quote",
			"(quote)",
			"",
			"quote: bad syntax",
		},
		{
			"empty begin in expression context",
			"(begin)",
			"",
			"begin: bad syntax",
		},
		{
			"empty begin in definition context",
			"(begin) 'foo",
			"'foo",
			"",
		},
		{
			"empty let*",
			"(let* ())",
			"",
			"begin: bad syntax",
		},
		{
			"empty lambda",
			"(lambda ())",
			"",
			"begin: bad syntax",
		},
		{
			"empty define procedure",
			"(define (id))",
			"",
			"begin: bad syntax",
		},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			environment := common.NewEnvironment(
				core.Bindings,
				nil,
			)
			var expected common.Datum
			if test.expected != "" {
				expectedBody, err := read.ReadString(test.expected)
				if err != nil {
					t.Fatal(err)
				}
				expected, err = ExpandBody(environment, expectedBody)
				if err != nil {
					t.Fatal(err)
				}
			}
			sourceBody, err := read.ReadString(test.source)
			if err != nil {
				t.Fatal(err)
			}
			actual, err := ExpandBody(environment, sourceBody)
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
			"lambda does not leak enclosing context",
			"((let* ((x 'foo)) (lambda () x))) x",
			"",
			"eval: unbound identifier x",
		},
		{
			"lambda does not leak arguments",
			"((lambda (x) x) 'foo) x",
			"",
			"eval: unbound identifier x",
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
			environment := common.NewEnvironment(
				core.Bindings,
				nil,
			)
			expression, err := ExpandBody(environment, body)
			if err != nil {
				t.Fatal(err)
			}
			actual, err := EvaluateExpressionOnce(environment, expression)
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
