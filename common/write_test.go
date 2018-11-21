package common_test

import (
	"testing"

	. "github.com/katsuya94/grime/common"
)

func TestWrite(t *testing.T) {
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
			"id",
		},
		{
			"empty list",
			Null,
			"()",
		},
		{
			"list with one symbol",
			Pair{Symbol("id"), Null},
			"(id)",
		},
		{
			"list with two symbols",
			Pair{Symbol("id"), Pair{Symbol("name"), Null}},
			"(id name)",
		},
		{
			"list with a literal",
			Pair{Boolean(false), Null},
			"(#f)",
		},
		{
			"improper list",
			Pair{Symbol("id"), Symbol("name")},
			"(id . name)",
		},
		{
			"empty list in a list",
			Pair{Null, Null},
			"(())",
		},
		{
			"list with one symbol in a list",
			Pair{Pair{Symbol("id"), Null}, Null},
			"((id))",
		},
		{
			"void",
			Void,
			"#<void>",
		},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			if actual := Write(test.datum); actual != test.expected {
				t.Fatalf("\nexpected: %v\n     got: %v", test.expected, actual)
			}
		})
	}
}
