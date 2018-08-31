package runtime_test

import (
	"reflect"
	"testing"

	"github.com/katsuya94/grime/read"
	. "github.com/katsuya94/grime/runtime"
)

func TestNewLibrary(t *testing.T) {
	tests := []struct {
		name     string
		source   string
		expected *Library
		error    string
	}{
		{
			"empty library",
			"(library (import) (export))",
			&Library{},
			"",
		},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			source, err := read.ReadString(test.source)
			if err != nil {
				t.Fatal(err)
			}
			actual, err := NewLibrary(source)
			if test.error != "" {
				if err == nil || err.Error() != test.error {
					t.Fatalf("\nexpected error: %v\n     got error: %v\n", test.error, err)
				}
			} else if err != nil {
				t.Fatal(err)
			} else if !reflect.DeepEqual(actual, test.expected) {
				t.Fatalf("\nexpected: %#v\n     got: %#v", test.expected, actual)
			}
		})
	}
}
