package eval

import (
	"testing"
	"github.com/katsuya94/grime/read"
	"github.com/katsuya94/grime/core"
	"reflect"
)

func TestMatch(t *testing.T) {
	tests := []struct {
		name     string
		literals map[core.Symbol]Binding
		pattern  string
		input    string
		ok       bool
		result   map[string]interface{}
		error    string
	}{
		{
			"pattern variable",
			nil,
			"id",
			"foo",
			true,
			map[string]interface{}{
				"id": "foo",
			},
			"",

		},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			data, err := read.ReadString(test.input)
			if err != nil {
				t.Fatal(err)
			} else if len(data) != 1 {
				t.Fatalf("encountered %v data in input", len(data))
			}
			input := data[0]
			data, err = read.ReadString(test.pattern)
			if err != nil {
				t.Fatal(err)
			} else if len(data) != 1 {
				t.Fatalf("encountered %v data in pattern", len(data))
			}
			pattern := data[0]
			expected := make(map[core.Symbol]interface{})
			for name, a := range test.result {
				switch s := a.(type) {
				case []string:
					expected[core.Symbol(name)] = []core.Datum{}
					for _, source := range(s) {
						data, err := read.ReadString(source)
						if err != nil {
							t.Fatal(err)
						} else if len(data) != 1 {
							t.Fatalf("encountered %v data in expected result RHS source", len(data))
						}
						expected[core.Symbol(name)] = append(expected[core.Symbol(name)].([]core.Datum), data[0])
					}
				case string:
					data, err := read.ReadString(s)
					if err != nil {
						t.Fatal(err)
					} else if len(data) != 1 {
						t.Fatalf("encountered %v data in expected result RHS source", len(data))
					}
					expected[core.Symbol(name)] = data[0]
				default:
					t.Fatalf("encountered malformed expected result RHS")
				}
			}
			result, ok, err := Match(input, pattern, test.literals)
			if test.error == "" && err != nil {
				t.Fatal(err)
			} else if test.error != "" && (err == nil || err.Error() != test.error) {
				t.Fatalf("\nexpected error: %v\n     got error: %v\n", test.error, err)
			} else if ok != test.ok {
				t.Fatalf("\nexpected ok: %v\n     got ok: %v\n", test.ok, ok)
			} else if !reflect.DeepEqual(result, expected) {
				t.Fatalf("\nexpected: %#v\n     got: %#v", expected, result)
			}
		})
	}
}

