package util_test

import (
	"reflect"
	"testing"

	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/read"
	. "github.com/katsuya94/grime/util"
)

func TestMatchSyntax(t *testing.T) {
	tests := []struct {
		name     string
		literals map[common.Symbol]common.Location
		pattern  string
		input    string
		ok       bool
		result   map[common.Symbol]interface{}
		error    string
	}{
		{
			"underscore",
			map[common.Symbol]common.Location{},
			"_",
			"foo",
			true,
			map[common.Symbol]interface{}{},
			"",
		},
		{
			"pattern variable matching identifier",
			map[common.Symbol]common.Location{},
			"id",
			"foo",
			true,
			map[common.Symbol]interface{}{
				common.Symbol("id"): common.NewWrappedSyntax(common.Symbol("foo")),
			},
			"",
		},
		{
			"pattern variable matching list",
			map[common.Symbol]common.Location{},
			"id",
			"(foo)",
			true,
			map[common.Symbol]interface{}{
				common.Symbol("id"): common.NewWrappedSyntax(common.Pair{common.Symbol("foo"), nil}),
			},
			"",
		},
		{
			"proper list",
			map[common.Symbol]common.Location{},
			"(id name)",
			"(foo bar)",
			true,
			map[common.Symbol]interface{}{
				common.Symbol("id"):   common.NewWrappedSyntax(common.Symbol("foo")),
				common.Symbol("name"): common.NewWrappedSyntax(common.Symbol("bar")),
			},
			"",
		},
		{
			"improper list",
			map[common.Symbol]common.Location{},
			"(id . name)",
			"(foo . bar)",
			true,
			map[common.Symbol]interface{}{
				common.Symbol("id"):   common.NewWrappedSyntax(common.Symbol("foo")),
				common.Symbol("name"): common.NewWrappedSyntax(common.Symbol("bar")),
			},
			"",
		},
		{
			"improper list matches list",
			map[common.Symbol]common.Location{},
			"(id . name)",
			"(foo bar)",
			true,
			map[common.Symbol]interface{}{
				common.Symbol("id"):   common.NewWrappedSyntax(common.Symbol("foo")),
				common.Symbol("name"): common.NewWrappedSyntax(common.Pair{common.Symbol("bar"), nil}),
			},
			"",
		},
		{
			"nested list",
			map[common.Symbol]common.Location{},
			"(id (name))",
			"(foo (bar))",
			true,
			map[common.Symbol]interface{}{
				common.Symbol("id"):   common.NewWrappedSyntax(common.Symbol("foo")),
				common.Symbol("name"): common.NewWrappedSyntax(common.Symbol("bar")),
			},
			"",
		},
		{
			"list ellipsis",
			map[common.Symbol]common.Location{},
			"(id ...)",
			"(foo bar)",
			true,
			map[common.Symbol]interface{}{
				common.Symbol("id"): []interface{}{
					common.NewWrappedSyntax(common.Symbol("foo")),
					common.NewWrappedSyntax(common.Symbol("bar")),
				},
			},
			"",
		},
		{
			"improper list ellipsis",
			map[common.Symbol]common.Location{},
			"(id ... . name)",
			"(foo bar . baz)",
			true,
			map[common.Symbol]interface{}{
				common.Symbol("id"): []interface{}{
					common.NewWrappedSyntax(common.Symbol("foo")),
					common.NewWrappedSyntax(common.Symbol("bar")),
				},
				common.Symbol("name"): common.NewWrappedSyntax(common.Symbol("baz")),
			},
			"",
		},
		{
			"list ellipsis with trailing",
			map[common.Symbol]common.Location{},
			"(id ... name)",
			"(foo bar baz)",
			true,
			map[common.Symbol]interface{}{
				common.Symbol("id"): []interface{}{
					common.NewWrappedSyntax(common.Symbol("foo")),
					common.NewWrappedSyntax(common.Symbol("bar")),
				},
				common.Symbol("name"): common.NewWrappedSyntax(common.Symbol("baz")),
			},
			"",
		},
		{
			"improper list ellipsis with trailing",
			map[common.Symbol]common.Location{},
			"(id ... name . key)",
			"(foo bar baz . qux)",
			true,
			map[common.Symbol]interface{}{
				common.Symbol("id"): []interface{}{
					common.NewWrappedSyntax(common.Symbol("foo")),
					common.NewWrappedSyntax(common.Symbol("bar")),
				},
				common.Symbol("name"): common.NewWrappedSyntax(common.Symbol("baz")),
				common.Symbol("key"):  common.NewWrappedSyntax(common.Symbol("qux")),
			},
			"",
		},
		{
			"list ellipsis with leading",
			map[common.Symbol]common.Location{},
			"(id name ...)",
			"(foo bar baz)",
			true,
			map[common.Symbol]interface{}{
				common.Symbol("id"): common.NewWrappedSyntax(common.Symbol("foo")),
				common.Symbol("name"): []interface{}{
					common.NewWrappedSyntax(common.Symbol("bar")),
					common.NewWrappedSyntax(common.Symbol("baz")),
				},
			},
			"",
		},
		{
			"improper list ellipsis with trailing",
			map[common.Symbol]common.Location{},
			"(id name ... . key)",
			"(foo bar baz . qux)",
			true,
			map[common.Symbol]interface{}{
				common.Symbol("id"): common.NewWrappedSyntax(common.Symbol("foo")),
				common.Symbol("name"): []interface{}{
					common.NewWrappedSyntax(common.Symbol("bar")),
					common.NewWrappedSyntax(common.Symbol("baz")),
				},
				common.Symbol("key"): common.NewWrappedSyntax(common.Symbol("qux")),
			},
			"",
		},
		{
			"nested list ellipsis",
			map[common.Symbol]common.Location{},
			"((id ...) ...)",
			"((foo) (bar baz))",
			true,
			map[common.Symbol]interface{}{
				common.Symbol("id"): []interface{}{
					[]interface{}{common.NewWrappedSyntax(common.Symbol("foo"))},
					[]interface{}{
						common.NewWrappedSyntax(common.Symbol("bar")),
						common.NewWrappedSyntax(common.Symbol("baz")),
					},
				},
			},
			"",
		},
		{
			"empty list ellipsis",
			map[common.Symbol]common.Location{},
			"(id ...)",
			"()",
			true,
			map[common.Symbol]interface{}{
				common.Symbol("id"): []interface{}{},
			},
			"",
		},
		{
			"list of tuples",
			map[common.Symbol]common.Location{},
			"((id name) ...)",
			"((foo bar) (baz qux))",
			true,
			map[common.Symbol]interface{}{
				common.Symbol("id"): []interface{}{
					common.NewWrappedSyntax(common.Symbol("foo")),
					common.NewWrappedSyntax(common.Symbol("baz")),
				},
				common.Symbol("name"): []interface{}{
					common.NewWrappedSyntax(common.Symbol("bar")),
					common.NewWrappedSyntax(common.Symbol("qux")),
				},
			},
			"",
		},
		{
			"boolean",
			map[common.Symbol]common.Location{},
			"#f",
			"#f",
			true,
			map[common.Symbol]interface{}{},
			"",
		},
		{
			"number",
			map[common.Symbol]common.Location{},
			"123",
			"123",
			true,
			map[common.Symbol]interface{}{},
			"",
		},
		{
			"character",
			map[common.Symbol]common.Location{},
			`#\x`,
			`#\x`,
			true,
			map[common.Symbol]interface{}{},
			"",
		},
		{
			"string",
			map[common.Symbol]common.Location{},
			`"name"`,
			`"name"`,
			true,
			map[common.Symbol]interface{}{},
			"",
		},
		{
			"empty list",
			map[common.Symbol]common.Location{},
			"()",
			"()",
			true,
			map[common.Symbol]interface{}{},
			"",
		},
		{
			"matching literal",
			map[common.Symbol]common.Location{common.Symbol("id"): nil},
			"id",
			"id",
			true,
			map[common.Symbol]interface{}{},
			"",
		},
		{
			"non-matching literal",
			map[common.Symbol]common.Location{common.Symbol("id"): nil},
			"id",
			"name",
			false,
			nil,
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
			input := common.NewWrappedSyntax(data[0])
			data, err = read.ReadString(test.pattern)
			if err != nil {
				t.Fatal(err)
			} else if len(data) != 1 {
				t.Fatalf("encountered %v data in pattern", len(data))
			}
			pattern := data[0]
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
