package eval

import (
	"testing"
	"github.com/katsuya94/grime/read"
	"github.com/katsuya94/grime/core"
	"reflect"
	"fmt"
)

func readMatchResult(shorthand interface{}) (interface{}, error) {
	switch v := shorthand.(type) {
	case []interface{}:
		var subresults []interface{}
		for _, subshorthand := range v {
			if subresult, err := readMatchResult(subshorthand); err != nil {
				return nil, err
			} else {
				subresults = append(subresults, subresult)
			}
		}
		return subresults, nil
	case string:
		if data, err := read.ReadString(v); err != nil {
			return nil, err
		} else if len(data) != 1 {
			return nil, fmt.Errorf("encountered %v data in expected result shorthand", len(data))
		} else {
			return data[0], nil
		}
	default:
		return nil, fmt.Errorf("unhandled shorthand: %#v", v)
	}
}

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
			"underscore",
			map[core.Symbol]Binding{},
			"_",
			"foo",
			true,
			map[string]interface{}{},
			"",
		},
		{
			"pattern variable matching symbol",
			map[core.Symbol]Binding{},
			"id",
			"foo",
			true,
			map[string]interface{}{
				"id": "foo",
			},
			"",
		},
		{
			"pattern variable matching list",
			map[core.Symbol]Binding{},
			"id",
			"(foo)",
			true,
			map[string]interface{}{
				"id": "(foo)",
			},
			"",
		},
		{
			"proper list",
			map[core.Symbol]Binding{},
			"(id name)",
			"(foo bar)",
			true,
			map[string]interface{}{
				"id": "foo",
				"name": "bar",
			},
			"",
		},
		{
			"improper list",
			map[core.Symbol]Binding{},
			"(id . name)",
			"(foo . bar)",
			true,
			map[string]interface{}{
				"id": "foo",
				"name": "bar",
			},
			"",
		},
		{
			"improper list matches list",
			map[core.Symbol]Binding{},
			"(id . name)",
			"(foo bar)",
			true,
			map[string]interface{}{
				"id": "foo",
				"name": "(bar)",
			},
			"",
		},
		{
			"nested list",
			map[core.Symbol]Binding{},
			"(id (name))",
			"(foo (bar))",
			true,
			map[string]interface{}{
				"id": "foo",
				"name": "bar",
			},
			"",
		},
		{
			"list ellipsis",
			map[core.Symbol]Binding{},
			"(id ...)",
			"(foo bar)",
			true,
			map[string]interface{}{
				"id": []interface{}{"foo", "bar"},
			},
			"",
		},
		{
			"improper list ellipsis",
			map[core.Symbol]Binding{},
			"(id ... . name)",
			"(foo bar . baz)",
			true,
			map[string]interface{}{
				"id": []interface{}{"foo", "bar"},
				"name": "baz",
			},
			"",
		},
		{
			"list ellipsis with trailing",
			map[core.Symbol]Binding{},
			"(id ... name)",
			"(foo bar baz)",
			true,
			map[string]interface{}{
				"id": []interface{}{"foo", "bar"},
				"name": "baz",
			},
			"",
		},
		{
			"improper list ellipsis with trailing",
			map[core.Symbol]Binding{},
			"(id ... name . key)",
			"(foo bar baz . qux)",
			true,
			map[string]interface{}{
				"id": []interface{}{"foo", "bar"},
				"name": "baz",
				"key": "qux",
			},
			"",
		},
		{
			"boolean",
			map[core.Symbol]Binding{},
			"#f",
			"#f",
			true,
			map[string]interface{}{},
			"",
		},
		{
			"number",
			map[core.Symbol]Binding{},
			"123",
			"123",
			true,
			map[string]interface{}{},
			"",
		},
		{
			"character",
			map[core.Symbol]Binding{},
			`#\x`,
			`#\x`,
			true,
			map[string]interface{}{},
			"",
		},
		{
			"string",
			map[core.Symbol]Binding{},
			`"name"`,
			`"name"`,
			true,
			map[string]interface{}{},
			"",
		},
		{
			"empty list",
			map[core.Symbol]Binding{},
			"()",
			"()",
			true,
			map[string]interface{}{},
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
			for name, shorthand := range test.result {
				if result, err := readMatchResult(shorthand); err != nil {
					t.Fatal(err)
				} else {
					expected[core.Symbol(name)] = result
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

