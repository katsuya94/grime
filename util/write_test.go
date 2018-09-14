package util_test

import (
	"testing"

	"github.com/katsuya94/grime/common"
	. "github.com/katsuya94/grime/util"
)

func TestWrite(t *testing.T) {
	tests := []struct {
		name     string
		datum    common.Datum
		expected string
	}{
		{
			"false",
			common.Boolean(false),
			"#f",
		},
		{
			"true",
			common.Boolean(true),
			"#t",
		},
		{
			"number",
			common.Number("123"),
			"123",
		},
		{
			"character",
			common.Character('a'),
			`#\a`,
		},
		{
			"string",
			common.String("name"),
			`"name"`,
		},
		{
			"symbol",
			common.Symbol("id"),
			"id",
		},
		{
			"empty list",
			nil,
			"()",
		},
		{
			"list with one symbol",
			common.Pair{common.Symbol("id"), nil},
			"(id)",
		},
		{
			"list with two symbols",
			common.Pair{common.Symbol("id"), common.Pair{common.Symbol("name"), nil}},
			"(id name)",
		},
		{
			"list with a literal",
			common.Pair{common.Boolean(false), nil},
			"(#f)",
		},
		{
			"improper list",
			common.Pair{common.Symbol("id"), common.Symbol("name")},
			"(id . name)",
		},
		{
			"empty list in a list",
			common.Pair{nil, nil},
			"(())",
		},
		{
			"list with one symbol in a list",
			common.Pair{common.Pair{common.Symbol("id"), nil}, nil},
			"((id))",
		},
		{
			"quote",
			common.Quote{common.Symbol("id")},
			"'id",
		},
		{
			"void",
			common.Void,
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
