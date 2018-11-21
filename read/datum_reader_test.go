package read

import (
	"reflect"
	"testing"

	"github.com/katsuya94/grime/common"
)

func TestRead(t *testing.T) {
	tests := []struct {
		name   string
		source string
		data   []common.Datum
		error  string
	}{
		{
			"boolean",
			"#f",
			[]common.Datum{common.Boolean(false)},
			"",
		},
		{
			"number",
			"123",
			[]common.Datum{common.Number("123")},
			"",
		},
		{
			"character",
			`#\x`,
			[]common.Datum{common.Character('x')},
			"",
		},
		{
			"string",
			`"name"`,
			[]common.Datum{common.String("name")},
			"",
		},
		{
			"symbol",
			"id",
			[]common.Datum{common.Symbol("id")},
			"",
		},
		{
			"two data",
			"id name",
			[]common.Datum{common.Symbol("id"), common.Symbol("name")},
			"",
		},
		{
			"empty list",
			"()",
			[]common.Datum{common.Null},
			"",
		},
		{
			"list with one element",
			"(id)",
			[]common.Datum{common.Pair{common.Symbol("id"), common.Null}},
			"",
		},
		{
			"list with two elements",
			"(id name)",
			[]common.Datum{common.Pair{common.Symbol("id"), common.Pair{common.Symbol("name"), common.Null}}},
			"",
		},
		{
			"pair",
			"(id . name)",
			[]common.Datum{common.Pair{common.Symbol("id"), common.Symbol("name")}},
			"",
		},
		{
			"quote",
			"'id",
			[]common.Datum{common.Pair{common.Symbol("quote"), common.Pair{common.Symbol("id"), common.Null}}},
			"",
		},
		{
			"quasiquote",
			"`id",
			[]common.Datum{common.Pair{common.Symbol("quasiquote"), common.Pair{common.Symbol("id"), common.Null}}},
			"",
		},
		{
			"unquote",
			",id",
			[]common.Datum{common.Pair{common.Symbol("unquote"), common.Pair{common.Symbol("id"), common.Null}}},
			"",
		},
		{
			"unquote splicing",
			",@id",
			[]common.Datum{common.Pair{common.Symbol("unquote-splicing"), common.Pair{common.Symbol("id"), common.Null}}},
			"",
		},
		{
			"syntax",
			"#'id",
			[]common.Datum{common.Pair{common.Symbol("syntax"), common.Pair{common.Symbol("id"), common.Null}}},
			"",
		},
		{
			"quasisyntax",
			"#`id",
			[]common.Datum{common.Pair{common.Symbol("quasisyntax"), common.Pair{common.Symbol("id"), common.Null}}},
			"",
		},
		{
			"unsyntax",
			"#,id",
			[]common.Datum{common.Pair{common.Symbol("unsyntax"), common.Pair{common.Symbol("id"), common.Null}}},
			"",
		},
		{
			"unsyntax splicing",
			"#,@id",
			[]common.Datum{common.Pair{common.Symbol("unsyntax-splicing"), common.Pair{common.Symbol("id"), common.Null}}},
			"",
		},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			data, err := ReadString(test.source)
			if test.error != "" {
				if err == nil || err.Error() != test.error {
					t.Fatalf("\nexpected error: %v\n     got error: %v\n", test.error, err)
				}
			} else if err != nil {
				t.Fatal(err)
			} else if !reflect.DeepEqual(data, test.data) {
				t.Fatalf("\nexpected: %#v\n     got: %#v", test.data, data)
			}
		})
	}
}
