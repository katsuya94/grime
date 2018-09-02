package runtime

import (
	"reflect"
	"testing"

	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/read"
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
			"(library (name) (export) (import))",
			&Library{
				name: []common.Symbol{common.Symbol("name")},
			},
			"",
		},
		{
			"empty library with multiple-identifier name",
			"(library (name id) (export) (import))",
			&Library{
				name: []common.Symbol{common.Symbol("name"), common.Symbol("id")},
			},
			"",
		},
		{
			"empty library with empty version",
			"(library (name ()) (export) (import))",
			&Library{
				name: []common.Symbol{common.Symbol("name")},
			},
			"",
		},
		{
			"empty library with single-number version",
			"(library (name (1)) (export) (import))",
			&Library{
				name:    []common.Symbol{common.Symbol("name")},
				version: []subVersion{1},
			},
			"",
		},
		{
			"empty library with multiple-number version",
			"(library (name (1 0)) (export) (import))",
			&Library{
				name:    []common.Symbol{common.Symbol("name")},
				version: []subVersion{1, 0},
			},
			"",
		},
		{
			"non-empty library",
			"(library (name) (export) (import) 'id)",
			&Library{
				name: []common.Symbol{common.Symbol("name")},
				body: []common.Datum{common.Pair{common.Symbol("quote"), common.Pair{common.Symbol("id"), nil}}},
			},
			"",
		},
		{
			"empty library with direct export",
			"(library (name) (export id) (import))",
			&Library{
				name: []common.Symbol{common.Symbol("name")},
				exportSpecs: []*identifierBinding{
					{common.Symbol("id"), common.Symbol("id")},
				},
			},
			"",
		},
		{
			"empty library with renamed export",
			"(library (name) (export (rename (id name))) (import))",
			&Library{
				name: []common.Symbol{common.Symbol("name")},
				exportSpecs: []*identifierBinding{
					{common.Symbol("id"), common.Symbol("name")},
				},
			},
			"",
		},
		{
			"empty library with multiple renamed exports",
			"(library (name) (export (rename (id name) (foo bar))) (import))",
			&Library{
				name: []common.Symbol{common.Symbol("name")},
				exportSpecs: []*identifierBinding{
					{common.Symbol("id"), common.Symbol("name")},
					{common.Symbol("foo"), common.Symbol("bar")},
				},
			},
			"",
		},
		{
			"empty library with renamed export and direct export",
			"(library (name) (export (rename (id name)) foo) (import))",
			&Library{
				name: []common.Symbol{common.Symbol("name")},
				exportSpecs: []*identifierBinding{
					{common.Symbol("id"), common.Symbol("name")},
					{common.Symbol("foo"), common.Symbol("foo")},
				},
			},
			"",
		},
		{
			"empty library with import",
			"(library (name) (export) (import base))",
			&Library{
				name: []common.Symbol{common.Symbol("name")},
				importSpecs: []*importSpec{
					{
						importSetLibraryReference{
							[]common.Symbol{common.Symbol("base")},
							versionReferenceSubVersionReferences{[]subVersionReference{}},
						},
						[]int{0},
					},
				},
			},
			"",
		},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			data, err := read.ReadString(test.source)
			if err != nil {
				t.Fatal(err)
			} else if len(data) != 1 {
				t.Fatalf("encountered %v data in source", len(data))
			}
			actual, err := NewLibrary(data[0])
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
