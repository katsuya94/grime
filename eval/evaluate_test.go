package eval

import (
	"github.com/katsuya94/grime/read"
	"reflect"
	"testing"
	"github.com/katsuya94/grime/core"
)

func TestEvaluate(t *testing.T) {
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
			"malformed quote",
			"(quote)",
			"",
			"eval: quote: malformed",
		},
		{
			"let",
			"(let ((x 'id)) x)",
			"id",
			"",
		},
		{
			"let multiple",
			"(let ((x 'id) (y 'name)) (list x y))",
			"(id name)",
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
			env := NewEnvironment()
			actual, err := env.EvaluateBody(body)
			if test.error == "" && err != nil {
				t.Fatal(err)
			} else if test.error != "" && (err == nil || err.Error() != test.error) {
				t.Fatalf("\nexpected error: %v\n     got error: %v\n", test.error, err)
			} else if !reflect.DeepEqual(actual, expected) {
				t.Fatalf("\nexpected: %#v\n     got: %#v", expected, actual)
			}
		})
	}
}
