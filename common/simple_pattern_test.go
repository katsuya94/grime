package common_test

import (
	"testing"

	. "github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/read"
	"github.com/katsuya94/grime/test"
	"github.com/stretchr/testify/require"
)

func TestMatchSyntax(t *testing.T) {
	tests := []struct {
		name     string
		literals []string
		pattern  string
		input    string
		ok       bool
		result   map[Symbol]interface{}
		error    string
	}{
		{
			"underscore",
			[]string{},
			"_",
			"foo",
			true,
			map[Symbol]interface{}{},
			"",
		},
		{
			"pattern variable matching identifier",
			[]string{},
			"id",
			"foo",
			true,
			map[Symbol]interface{}{
				Symbol("id"): test.Syntax("foo"),
			},
			"",
		},
		{
			"pattern variable matching list",
			[]string{},
			"id",
			"(foo)",
			true,
			map[Symbol]interface{}{
				Symbol("id"): test.Syntax("(foo)"),
			},
			"",
		},
		{
			"proper list",
			[]string{},
			"(id name)",
			"(foo bar)",
			true,
			map[Symbol]interface{}{
				Symbol("id"):   test.Syntax("foo"),
				Symbol("name"): test.Syntax("bar"),
			},
			"",
		},
		{
			"improper list",
			[]string{},
			"(id . name)",
			"(foo . bar)",
			true,
			map[Symbol]interface{}{
				Symbol("id"):   test.Syntax("foo"),
				Symbol("name"): test.Syntax("bar"),
			},
			"",
		},
		{
			"improper list matches list",
			[]string{},
			"(id . name)",
			"(foo bar)",
			true,
			map[Symbol]interface{}{
				Symbol("id"):   test.Syntax("foo"),
				Symbol("name"): test.Syntax("(bar)"),
			},
			"",
		},
		{
			"nested list",
			[]string{},
			"(id (name))",
			"(foo (bar))",
			true,
			map[Symbol]interface{}{
				Symbol("id"):   test.Syntax("foo"),
				Symbol("name"): test.Syntax("bar"),
			},
			"",
		},
		{
			"list ellipsis",
			[]string{},
			"(id ...)",
			"(foo bar)",
			true,
			map[Symbol]interface{}{
				Symbol("id"): []interface{}{
					test.Syntax("foo"),
					test.Syntax("bar"),
				},
			},
			"",
		},
		{
			"improper list ellipsis",
			[]string{},
			"(id ... . name)",
			"(foo bar . baz)",
			true,
			map[Symbol]interface{}{
				Symbol("id"): []interface{}{
					test.Syntax("foo"),
					test.Syntax("bar"),
				},
				Symbol("name"): test.Syntax("baz"),
			},
			"",
		},
		{
			"list ellipsis with trailing",
			[]string{},
			"(id ... name)",
			"(foo bar baz)",
			true,
			map[Symbol]interface{}{
				Symbol("id"): []interface{}{
					test.Syntax("foo"),
					test.Syntax("bar"),
				},
				Symbol("name"): test.Syntax("baz"),
			},
			"",
		},
		{
			"improper list ellipsis with trailing",
			[]string{},
			"(id ... name . key)",
			"(foo bar baz . qux)",
			true,
			map[Symbol]interface{}{
				Symbol("id"): []interface{}{
					test.Syntax("foo"),
					test.Syntax("bar"),
				},
				Symbol("name"): test.Syntax("baz"),
				Symbol("key"):  test.Syntax("qux"),
			},
			"",
		},
		{
			"list ellipsis with leading",
			[]string{},
			"(id name ...)",
			"(foo bar baz)",
			true,
			map[Symbol]interface{}{
				Symbol("id"): test.Syntax("foo"),
				Symbol("name"): []interface{}{
					test.Syntax("bar"),
					test.Syntax("baz"),
				},
			},
			"",
		},
		{
			"improper list ellipsis with trailing",
			[]string{},
			"(id name ... . key)",
			"(foo bar baz . qux)",
			true,
			map[Symbol]interface{}{
				Symbol("id"): test.Syntax("foo"),
				Symbol("name"): []interface{}{
					test.Syntax("bar"),
					test.Syntax("baz"),
				},
				Symbol("key"): test.Syntax("qux"),
			},
			"",
		},
		{
			"nested list ellipsis",
			[]string{},
			"((id ...) ...)",
			"((foo) (bar baz))",
			true,
			map[Symbol]interface{}{
				Symbol("id"): []interface{}{
					[]interface{}{
						test.Syntax("foo"),
					},
					[]interface{}{
						test.Syntax("bar"),
						test.Syntax("baz"),
					},
				},
			},
			"",
		},
		{
			"empty list ellipsis",
			[]string{},
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
			[]string{},
			"((id name) ...)",
			"((foo bar) (baz qux))",
			true,
			map[Symbol]interface{}{
				Symbol("id"): []interface{}{
					test.Syntax("foo"),
					test.Syntax("baz"),
				},
				Symbol("name"): []interface{}{
					test.Syntax("bar"),
					test.Syntax("qux"),
				},
			},
			"",
		},
		{
			"boolean",
			[]string{},
			"#f",
			"#f",
			true,
			map[Symbol]interface{}{},
			"",
		},
		{
			"number",
			[]string{},
			"123",
			"123",
			true,
			map[Symbol]interface{}{},
			"",
		},
		{
			"character",
			[]string{},
			`#\x`,
			`#\x`,
			true,
			map[Symbol]interface{}{},
			"",
		},
		{
			"string",
			[]string{},
			`"name"`,
			`"name"`,
			true,
			map[Symbol]interface{}{},
			"",
		},
		{
			"empty list",
			[]string{},
			"()",
			"()",
			true,
			map[Symbol]interface{}{},
			"",
		},
		{
			"matching literal",
			[]string{"id"},
			"id",
			"id",
			true,
			map[Symbol]interface{}{},
			"",
		},
		{
			"non-matching literal",
			[]string{"id"},
			"id",
			"name",
			false,
			nil,
			"",
		},
	}
	for _, _test := range tests {
		t.Run(_test.name, func(t *testing.T) {
			input := test.Syntax(_test.input)
			literals := []Symbol{}
			for _, s := range _test.literals {
				literals = append(literals, Symbol(s))
			}
			pattern, err := CompileSimplePattern(read.MustReadDatum(_test.pattern), literals...)
			if _test.error != "" {
				require.EqualError(t, err, _test.error)
			}
			result, ok := pattern.Match(input)
			if _test.ok {
				require.True(t, ok)
				require.Equal(t, _test.result, result)
			} else {
				require.False(t, ok)
			}
		})
	}
}
