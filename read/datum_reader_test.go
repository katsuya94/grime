package read

import (
	"github.com/katsuya94/grime/core"
	"reflect"
	"testing"
)

func TestRead(t *testing.T) {
	tests := []struct {
		name   string
		source string
		data   []core.Datum
		error  string
	}{
		{
			"boolean",
			"#f",
			[]core.Datum{core.Boolean(false)},
			"",
		},
		{
			"number",
			"123",
			[]core.Datum{core.Number("123")},
			"",
		},
		{
			"character",
			`#\x`,
			[]core.Datum{core.Character('x')},
			"",
		},
		{
			"string",
			`"name"`,
			[]core.Datum{core.String("name")},
			"",
		},
		{
			"symbol",
			"id",
			[]core.Datum{core.Symbol("id")},
			"",
		},
		{
			"two data",
			"id name",
			[]core.Datum{core.Symbol("id"), core.Symbol("name")},
			"",
		},
		{
			"empty list",
			"()",
			[]core.Datum{nil},
			"",
		},
		{
			"list with one element",
			"(id)",
			[]core.Datum{core.Pair{core.Symbol("id"), nil}},
			"",
		},
		{
			"list with two elements",
			"(id name)",
			[]core.Datum{core.Pair{core.Symbol("id"), core.Pair{core.Symbol("name"), nil}}},
			"",
		},
		{
			"pair",
			"(id . name)",
			[]core.Datum{core.Pair{core.Symbol("id"), core.Symbol("name")}},
			"",
		},
		{
			"quote",
			"'id",
			[]core.Datum{core.Pair{core.Symbol("quote"), core.Pair{core.Symbol("id"), nil}}},
			"",
		},
		{
			"quasiquote",
			"`id",
			[]core.Datum{core.Pair{core.Symbol("quasiquote"), core.Pair{core.Symbol("id"), nil}}},
			"",
		},
		{
			"unquote",
			",id",
			[]core.Datum{core.Pair{core.Symbol("unquote"), core.Pair{core.Symbol("id"), nil}}},
			"",
		},
		{
			"unquote splicing",
			",@id",
			[]core.Datum{core.Pair{core.Symbol("unquote-splicing"), core.Pair{core.Symbol("id"), nil}}},
			"",
		},
		{
			"syntax",
			"#'id",
			[]core.Datum{core.Pair{core.Symbol("syntax"), core.Pair{core.Symbol("id"), nil}}},
			"",
		},
		{
			"quasisyntax",
			"#`id",
			[]core.Datum{core.Pair{core.Symbol("quasisyntax"), core.Pair{core.Symbol("id"), nil}}},
			"",
		},
		{
			"unsyntax",
			"#,id",
			[]core.Datum{core.Pair{core.Symbol("unsyntax"), core.Pair{core.Symbol("id"), nil}}},
			"",
		},
		{
			"unsyntax splicing",
			"#,@id",
			[]core.Datum{core.Pair{core.Symbol("unsyntax-splicing"), core.Pair{core.Symbol("id"), nil}}},
			"",
		},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			data, err := ReadString(test.source)
			if test.error == "" && err != nil {
				t.Fatal(err)
			} else if test.error != "" && (err == nil || err.Error() != test.error) {
				t.Fatalf("\nexpected error: %v\n     got error: %v\n", test.error, err)
			} else if !reflect.DeepEqual(data, test.data) {
				t.Fatalf("\nexpected: %#v\n     got: %#v", test.data, data)
			}
		})
	}
}
