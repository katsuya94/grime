package read

import (
	"fmt"
	"reflect"
	"strings"
	"testing"

	"github.com/katsuya94/grime/common"
)

func slt(sourceLocation common.SourceLocation, children common.Datum) common.SourceLocationTree {
	return common.SourceLocationTree{
		sourceLocation,
		children,
	}
}

func TestRead(t *testing.T) {
	tests := []struct {
		name                string
		source              string
		data                []common.Datum
		sourceLocationTrees []common.SourceLocationTree
		error               string
	}{
		{
			"boolean",
			"#f",
			[]common.Datum{common.Boolean(false)},
			[]common.SourceLocationTree{slt(sl("0:0:0:2"), nil)},
			"",
		},
		{
			"number",
			"123",
			[]common.Datum{common.Number("123")},
			[]common.SourceLocationTree{slt(sl("0:0:0:3"), nil)},
			"",
		},
		{
			"character",
			`#\x`,
			[]common.Datum{common.Character('x')},
			[]common.SourceLocationTree{slt(sl("0:0:0:3"), nil)},
			"",
		},
		{
			"string",
			`"name"`,
			[]common.Datum{common.String("name")},
			[]common.SourceLocationTree{slt(sl("0:0:0:6"), nil)},
			"",
		},
		{
			"symbol",
			"id",
			[]common.Datum{common.Symbol("id")},
			[]common.SourceLocationTree{slt(sl("0:0:0:2"), nil)},
			"",
		},
		{
			"two data",
			"id name",
			[]common.Datum{common.Symbol("id"), common.Symbol("name")},
			[]common.SourceLocationTree{slt(sl("0:0:0:2"), nil), slt(sl("0:3:3:4"), nil)},
			"",
		},
		{
			"empty list",
			"()",
			[]common.Datum{common.Null},
			[]common.SourceLocationTree{slt(sl("0:0:0:2"), nil)},
			"",
		},
		{
			"list with one element",
			"(id)",
			[]common.Datum{common.Pair{common.Symbol("id"), common.Null}},
			[]common.SourceLocationTree{slt(sl("0:0:0:4"), common.Pair{slt(sl("0:1:1:2"), nil), slt(sl("0:3:3:1"), nil)})},
			"",
		},
		{
			"list with two elements",
			"(id name)",
			[]common.Datum{common.Pair{common.Symbol("id"), common.Pair{common.Symbol("name"), common.Null}}},
			[]common.SourceLocationTree{slt(sl("0:0:0:9"), common.Pair{slt(sl("0:1:1:2"), nil), slt(sl("0:4:4:5"), common.Pair{slt(sl("0:4:4:4"), nil), slt(sl("0:8:8:1"), nil)})})},
			"",
		},
		{
			"pair",
			"(id . name)",
			[]common.Datum{common.Pair{common.Symbol("id"), common.Symbol("name")}},
			[]common.SourceLocationTree{slt(sl("0:0:0:11"), common.Pair{slt(sl("0:1:1:2"), nil), slt(sl("0:6:6:4"), nil)})},
			"",
		},
		{
			"quote",
			"'id",
			[]common.Datum{common.Pair{common.Symbol("quote"), common.Pair{common.Symbol("id"), common.Null}}},
			[]common.SourceLocationTree{slt(sl("0:0:0:3"), common.Pair{slt(sl("0:0:0:1"), nil), slt(sl("0:0:0:1"), common.Pair{slt(sl("0:1:1:2"), nil), slt(sl("0:0:0:1"), nil)})})},
			"",
		},
		{
			"quasiquote",
			"`id",
			[]common.Datum{common.Pair{common.Symbol("quasiquote"), common.Pair{common.Symbol("id"), common.Null}}},
			[]common.SourceLocationTree{slt(sl("0:0:0:3"), common.Pair{slt(sl("0:0:0:1"), nil), slt(sl("0:0:0:1"), common.Pair{slt(sl("0:1:1:2"), nil), slt(sl("0:0:0:1"), nil)})})},
			"",
		},
		{
			"unquote",
			",id",
			[]common.Datum{common.Pair{common.Symbol("unquote"), common.Pair{common.Symbol("id"), common.Null}}},
			[]common.SourceLocationTree{slt(sl("0:0:0:3"), common.Pair{slt(sl("0:0:0:1"), nil), slt(sl("0:0:0:1"), common.Pair{slt(sl("0:1:1:2"), nil), slt(sl("0:0:0:1"), nil)})})},
			"",
		},
		{
			"unquote splicing",
			",@id",
			[]common.Datum{common.Pair{common.Symbol("unquote-splicing"), common.Pair{common.Symbol("id"), common.Null}}},
			[]common.SourceLocationTree{slt(sl("0:0:0:4"), common.Pair{slt(sl("0:0:0:2"), nil), slt(sl("0:0:0:2"), common.Pair{slt(sl("0:2:2:2"), nil), slt(sl("0:0:0:2"), nil)})})},
			"",
		},
		{
			"syntax",
			"#'id",
			[]common.Datum{common.Pair{common.Symbol("syntax"), common.Pair{common.Symbol("id"), common.Null}}},
			[]common.SourceLocationTree{slt(sl("0:0:0:4"), common.Pair{slt(sl("0:0:0:2"), nil), slt(sl("0:0:0:2"), common.Pair{slt(sl("0:2:2:2"), nil), slt(sl("0:0:0:2"), nil)})})},
			"",
		},
		{
			"quasisyntax",
			"#`id",
			[]common.Datum{common.Pair{common.Symbol("quasisyntax"), common.Pair{common.Symbol("id"), common.Null}}},
			[]common.SourceLocationTree{slt(sl("0:0:0:4"), common.Pair{slt(sl("0:0:0:2"), nil), slt(sl("0:0:0:2"), common.Pair{slt(sl("0:2:2:2"), nil), slt(sl("0:0:0:2"), nil)})})},
			"",
		},
		{
			"unsyntax",
			"#,id",
			[]common.Datum{common.Pair{common.Symbol("unsyntax"), common.Pair{common.Symbol("id"), common.Null}}},
			[]common.SourceLocationTree{slt(sl("0:0:0:4"), common.Pair{slt(sl("0:0:0:2"), nil), slt(sl("0:0:0:2"), common.Pair{slt(sl("0:2:2:2"), nil), slt(sl("0:0:0:2"), nil)})})},
			"",
		},
		{
			"unsyntax splicing",
			"#,@id",
			[]common.Datum{common.Pair{common.Symbol("unsyntax-splicing"), common.Pair{common.Symbol("id"), common.Null}}},
			[]common.SourceLocationTree{slt(sl("0:0:0:5"), common.Pair{slt(sl("0:0:0:3"), nil), slt(sl("0:0:0:3"), common.Pair{slt(sl("0:3:3:2"), nil), slt(sl("0:0:0:3"), nil)})})},
			"",
		},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			syntaxes, nullSourceLocationTree, err := Read("string", strings.NewReader(test.source))
			if test.error != "" {
				if err == nil || err.Error() != test.error {
					t.Fatalf("\nexpected error: %v\n     got error: %v\n", test.error, err)
				}
			} else if err != nil {
				t.Fatal(err)
			} else {
				var (
					data                []common.Datum
					sourceLocationTrees []common.SourceLocationTree
				)
				for _, syntax := range syntaxes {
					data = append(data, syntax.Unwrap())
					sourceLocationTrees = append(sourceLocationTrees, *syntax.SourceLocationTree())
				}
				if !reflect.DeepEqual(data, test.data) {
					t.Errorf("\nexpected: %#v\n     got: %#v", test.data, data)
				}
				if !reflect.DeepEqual(sourceLocationTrees, test.sourceLocationTrees) {
					t.Errorf("\nexpected: %#v\n     got: %#v", test.sourceLocationTrees, sourceLocationTrees)
				}
				expectedNullSourceLocationTree := slt(sl(fmt.Sprintf("0:%d:%d:0", len(test.source)+1, len(test.source)+1)), nil)
				if !reflect.DeepEqual(nullSourceLocationTree, expectedNullSourceLocationTree) {
					t.Errorf("\nexpected: %#v\n     got: %#v", expectedNullSourceLocationTree, nullSourceLocationTree)
				}
			}
		})
	}
}
