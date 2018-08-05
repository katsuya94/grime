package read

import (
	"reflect"
	"testing"
)

func TestRead(t *testing.T) {
	tests := []struct {
		name   string
		source string
		data   []Datum
		error  string
	}{
		{
			"boolean",
			"#f",
			[]Datum{Boolean(false)},
			"",
		},
		{
			"number",
			"123",
			[]Datum{Number("123")},
			"",
		},
		{
			"character",
			`#\x`,
			[]Datum{Character('x')},
			"",
		},
		{
			"string",
			`"name"`,
			[]Datum{String("name")},
			"",
		},
		{
			"symbol",
			"id",
			[]Datum{Symbol("id")},
			"",
		},
		{
			"two data",
			"id name",
			[]Datum{Symbol("id"), Symbol("name")},
			"",
		},
		{
			"empty list",
			"()",
			[]Datum{nil},
			"",
		},
		{
			"list with one element",
			"(id)",
			[]Datum{Pair{Symbol("id"), nil}},
			"",
		},
		{
			"list with two elements",
			"(id name)",
			[]Datum{Pair{Symbol("id"), Pair{Symbol("name"), nil}}},
			"",
		},
		{
			"pair",
			"(id . name)",
			[]Datum{Pair{Symbol("id"), Symbol("name")}},
			"",
		},
		{
			"quote",
			"'id",
			[]Datum{Pair{Symbol("quote"), Pair{Symbol("id"), nil}}},
			"",
		},
		{
			"quasiquote",
			"`id",
			[]Datum{Pair{Symbol("quasiquote"), Pair{Symbol("id"), nil}}},
			"",
		},
		{
			"unquote",
			",id",
			[]Datum{Pair{Symbol("unquote"), Pair{Symbol("id"), nil}}},
			"",
		},
		{
			"unquote splicing",
			",@id",
			[]Datum{Pair{Symbol("unquote-splicing"), Pair{Symbol("id"), nil}}},
			"",
		},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			data, err := ReadString(test.source)
			if test.error == "" && err != nil {
				t.Fatal(err)
			} else if test.error != "" && err != nil && err.Error() != test.error {
				t.Errorf("\nexpected error: %v\n     got error: %v\n", test.error, err.Error())
			}
			if test.data != nil && !reflect.DeepEqual(data, test.data) {
				t.Errorf("\nexpected: %#v\n     got: %#v", test.data, data)
			}
		})
	}
}
