package common_test

import (
	"reflect"
	"testing"

	. "github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/read"
)

func TestMatchSyntax(t *testing.T) {
	tests := []struct {
		name     string
		literals map[Symbol]Location
		pattern  string
		input    string
		ok       bool
		result   map[Symbol]interface{}
		error    string
	}{
		{
			"underscore",
			map[Symbol]Location{},
			"_",
			"foo",
			true,
			map[Symbol]interface{}{},
			"",
		},
		{
			"pattern variable matching identifier",
			map[Symbol]Location{},
			"id",
			"foo",
			true,
			map[Symbol]interface{}{
				Symbol("id"): NewWrappedSyntax(Symbol("foo")),
			},
			"",
		},
		{
			"pattern variable matching list",
			map[Symbol]Location{},
			"id",
			"(foo)",
			true,
			map[Symbol]interface{}{
				Symbol("id"): NewWrappedSyntax(Pair{Symbol("foo"), Null}),
			},
			"",
		},
		{
			"proper list",
			map[Symbol]Location{},
			"(id name)",
			"(foo bar)",
			true,
			map[Symbol]interface{}{
				Symbol("id"):   NewWrappedSyntax(Symbol("foo")),
				Symbol("name"): NewWrappedSyntax(Symbol("bar")),
			},
			"",
		},
		{
			"improper list",
			map[Symbol]Location{},
			"(id . name)",
			"(foo . bar)",
			true,
			map[Symbol]interface{}{
				Symbol("id"):   NewWrappedSyntax(Symbol("foo")),
				Symbol("name"): NewWrappedSyntax(Symbol("bar")),
			},
			"",
		},
		{
			"improper list matches list",
			map[Symbol]Location{},
			"(id . name)",
			"(foo bar)",
			true,
			map[Symbol]interface{}{
				Symbol("id"):   NewWrappedSyntax(Symbol("foo")),
				Symbol("name"): NewWrappedSyntax(Pair{Symbol("bar"), Null}),
			},
			"",
		},
		{
			"nested list",
			map[Symbol]Location{},
			"(id (name))",
			"(foo (bar))",
			true,
			map[Symbol]interface{}{
				Symbol("id"):   NewWrappedSyntax(Symbol("foo")),
				Symbol("name"): NewWrappedSyntax(Symbol("bar")),
			},
			"",
		},
		{
			"list ellipsis",
			map[Symbol]Location{},
			"(id ...)",
			"(foo bar)",
			true,
			map[Symbol]interface{}{
				Symbol("id"): []interface{}{
					NewWrappedSyntax(Symbol("foo")),
					NewWrappedSyntax(Symbol("bar")),
				},
			},
			"",
		},
		{
			"improper list ellipsis",
			map[Symbol]Location{},
			"(id ... . name)",
			"(foo bar . baz)",
			true,
			map[Symbol]interface{}{
				Symbol("id"): []interface{}{
					NewWrappedSyntax(Symbol("foo")),
					NewWrappedSyntax(Symbol("bar")),
				},
				Symbol("name"): NewWrappedSyntax(Symbol("baz")),
			},
			"",
		},
		{
			"list ellipsis with trailing",
			map[Symbol]Location{},
			"(id ... name)",
			"(foo bar baz)",
			true,
			map[Symbol]interface{}{
				Symbol("id"): []interface{}{
					NewWrappedSyntax(Symbol("foo")),
					NewWrappedSyntax(Symbol("bar")),
				},
				Symbol("name"): NewWrappedSyntax(Symbol("baz")),
			},
			"",
		},
		{
			"improper list ellipsis with trailing",
			map[Symbol]Location{},
			"(id ... name . key)",
			"(foo bar baz . qux)",
			true,
			map[Symbol]interface{}{
				Symbol("id"): []interface{}{
					NewWrappedSyntax(Symbol("foo")),
					NewWrappedSyntax(Symbol("bar")),
				},
				Symbol("name"): NewWrappedSyntax(Symbol("baz")),
				Symbol("key"):  NewWrappedSyntax(Symbol("qux")),
			},
			"",
		},
		{
			"list ellipsis with leading",
			map[Symbol]Location{},
			"(id name ...)",
			"(foo bar baz)",
			true,
			map[Symbol]interface{}{
				Symbol("id"): NewWrappedSyntax(Symbol("foo")),
				Symbol("name"): []interface{}{
					NewWrappedSyntax(Symbol("bar")),
					NewWrappedSyntax(Symbol("baz")),
				},
			},
			"",
		},
		{
			"improper list ellipsis with trailing",
			map[Symbol]Location{},
			"(id name ... . key)",
			"(foo bar baz . qux)",
			true,
			map[Symbol]interface{}{
				Symbol("id"): NewWrappedSyntax(Symbol("foo")),
				Symbol("name"): []interface{}{
					NewWrappedSyntax(Symbol("bar")),
					NewWrappedSyntax(Symbol("baz")),
				},
				Symbol("key"): NewWrappedSyntax(Symbol("qux")),
			},
			"",
		},
		{
			"nested list ellipsis",
			map[Symbol]Location{},
			"((id ...) ...)",
			"((foo) (bar baz))",
			true,
			map[Symbol]interface{}{
				Symbol("id"): []interface{}{
					[]interface{}{NewWrappedSyntax(Symbol("foo"))},
					[]interface{}{
						NewWrappedSyntax(Symbol("bar")),
						NewWrappedSyntax(Symbol("baz")),
					},
				},
			},
			"",
		},
		{
			"empty list ellipsis",
			map[Symbol]Location{},
			"(id ...)",
			"()",
			true,
			map[Symbol]interface{}{
				Symbol("id"): []interface{}{},
			},
			"",
		},
		{
			"list of tuples",
			map[Symbol]Location{},
			"((id name) ...)",
			"((foo bar) (baz qux))",
			true,
			map[Symbol]interface{}{
				Symbol("id"): []interface{}{
					NewWrappedSyntax(Symbol("foo")),
					NewWrappedSyntax(Symbol("baz")),
				},
				Symbol("name"): []interface{}{
					NewWrappedSyntax(Symbol("bar")),
					NewWrappedSyntax(Symbol("qux")),
				},
			},
			"",
		},
		{
			"boolean",
			map[Symbol]Location{},
			"#f",
			"#f",
			true,
			map[Symbol]interface{}{},
			"",
		},
		{
			"number",
			map[Symbol]Location{},
			"123",
			"123",
			true,
			map[Symbol]interface{}{},
			"",
		},
		{
			"character",
			map[Symbol]Location{},
			`#\x`,
			`#\x`,
			true,
			map[Symbol]interface{}{},
			"",
		},
		{
			"string",
			map[Symbol]Location{},
			`"name"`,
			`"name"`,
			true,
			map[Symbol]interface{}{},
			"",
		},
		{
			"empty list",
			map[Symbol]Location{},
			"()",
			"()",
			true,
			map[Symbol]interface{}{},
			"",
		},
		{
			"matching literal",
			map[Symbol]Location{Symbol("id"): nil},
			"id",
			"id",
			true,
			map[Symbol]interface{}{},
			"",
		},
		{
			"non-matching literal",
			map[Symbol]Location{Symbol("id"): nil},
			"id",
			"name",
			false,
			nil,
			"",
		},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			input := NewWrappedSyntax(read.MustReadDatum(test.input))
			pattern := Pattern(read.MustReadDatum(test.pattern))
			result, ok, err := MatchSyntax(input, pattern, test.literals)
			if test.error == "" && err != nil {
				t.Fatal(err)
			} else if test.error != "" {
				if err == nil || err.Error() != test.error {
					t.Fatalf("\nexpected error: %v\n     got error: %v\n", test.error, err)
				}
			} else if ok != test.ok {
				t.Fatalf("\nexpected ok: %v\n     got ok: %v\n", test.ok, ok)
			} else if !reflect.DeepEqual(result, test.result) {
				t.Fatalf("\nexpected: %#v\n     got: %#v", test.result, result)
			}
		})
	}
}
