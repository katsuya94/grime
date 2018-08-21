package eval

import (
	"github.com/katsuya94/grime/core"
	"github.com/katsuya94/grime/read"
	"reflect"
	"testing"
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
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			env := NewEnvironment()
			var expected core.Datum
			if test.expected != "" {
				expectedBody, err := read.ReadString(test.expected)
				if err != nil {
					t.Fatal(err)
				}
				expected, err = ExpandBody(env, expectedBody)
				if err != nil {
					t.Fatal(err)
				}
			}
			sourceBody, err := read.ReadString(test.source)
			if err != nil {
				t.Fatal(err)
			}
			actual, err := ExpandBody(env, sourceBody)
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
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			data, err := read.ReadString(test.val)
			if err != nil {
				t.Fatal(err)
			} else if len(data) == 0 {
				data = []core.Datum{nil}
			}
			expected := data[0]
			body, err := read.ReadString(test.source)
			if err != nil {
				t.Fatal(err)
			}
			env := NewEnvironment()
			expression, err := ExpandBody(env, body)
			if err != nil {
				t.Fatal(err)
			}
			actual, err := EvaluateExpression(env, expression)
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
