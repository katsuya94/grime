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
