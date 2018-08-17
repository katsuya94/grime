package core

import "testing"

func TestQuote(t *testing.T) {
	tests := []struct {
		name     string
		datum    Datum
		expected string
	}{
		{
			"false",
			Boolean(false),
			"#f",
		},
		{
			"true",
			Boolean(true),
			"#t",
		},
		{
			"number",
			Number("123"),
			"123",
		},
		{
			"character",
			Character('a'),
			`#\a`,
		},
		{
			"string",
			String("name"),
			`"name"`,
		},
		{
			"symbol",
			Symbol("id"),
			"'id",
		},
		{
			"empty list",
			nil,
			"'()",
		},
		{
			"list with one symbol",
			Pair{Symbol("id"), nil},
			"'(id)",
		},
		{
			"list with two symbols",
			Pair{Symbol("id"), Pair{Symbol("name"), nil}},
			"'(id name)",
		},
		{
			"list with a literal",
			Pair{Boolean(false), nil},
			"'(#f)",
		},
		{
			"improper list",
			Pair{Symbol("id"), Symbol("name")},
			"'(id . name)",
		},
		{
			"empty list in a list",
			Pair{nil, nil},
			"'(())",
		},
		{
			"list with one symbol in a list",
			Pair{Pair{Symbol("id"), nil}, nil},
			"'((id))",
		},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			if actual := Display(test.datum); actual != test.expected {
				t.Fatalf("\nexpected: %v\n     got: %v", test.expected, actual)
			}
		})
	}
}
