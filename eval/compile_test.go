package eval_test

import (
	"reflect"
	"testing"

	"github.com/katsuya94/grime/common"
	. "github.com/katsuya94/grime/eval"
	"github.com/katsuya94/grime/lib/core"
	"github.com/katsuya94/grime/read"
)

func TestCompile(t *testing.T) {
	tests := []struct {
		name     string
		source   string
		expected string
		error    string
	}{
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
			"compile: begin: empty in expression context",
		},
		{
			"empty lambda",
			"(lambda ())",
			"",
			"compile: begin: empty in expression context",
		},
		{
			"empty define procedure",
			"(define (id))",
			"",
			"compile: begin: empty in expression context",
		},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			environment := common.NewEnvironment(
				core.Bindings,
				nil,
			)
			var expected common.Expression
			if test.expected != "" {
				expectedBody, err := read.ReadString(test.expected)
				if err != nil {
					t.Fatal(err)
				}
				expectedForm, err := ExpandBody(environment, expectedBody)
				if err != nil {
					t.Fatal(err)
				}
				expected, err = Compile(environment, expectedForm)
				if err != nil {
					t.Fatal(err)
				}
			}
			sourceBody, err := read.ReadString(test.source)
			if err != nil {
				t.Fatal(err)
			}
			form, err := ExpandBody(environment, sourceBody)
			if err != nil {
				t.Fatal(err)
			}
			actual, err := Compile(environment, form)
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
