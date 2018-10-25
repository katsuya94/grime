package runtime

import (
	"reflect"
	"testing"

	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/read"
)

func TestImportSpec_resolve(t *testing.T) {
	library := &Library{
		name:    []common.Symbol{common.Symbol("rnrs"), common.Symbol("base")},
		version: []subVersion{6, 4},
	}
	tests := []struct {
		name       string
		source     string
		resolution importSpecResolution
		ok         bool
	}{
		{
			"name",
			"(rnrs base)",
			importSpecResolution{
				importSetResolution{
					identifierSpecAll{},
				},
				[]int{0},
			},
			true,
		},
		{
			"partial name",
			"(rnrs)",
			importSpecResolution{},
			false,
		},
		{
			"name with levels",
			"(for (rnrs base) expand)",
			importSpecResolution{
				importSetResolution{
					identifierSpecAll{},
				},
				[]int{1},
			},
			true,
		},
		{
			"only",
			"(only (rnrs base) id)",
			importSpecResolution{
				importSetResolution{
					identifierSpecOnly{
						identifierSpecAll{},
						[]common.Symbol{common.Symbol("id")},
					},
				},
				[]int{0},
			},
			true,
		},
		{
			"except",
			"(except (rnrs base) id)",
			importSpecResolution{
				importSetResolution{
					identifierSpecExcept{
						identifierSpecAll{},
						[]common.Symbol{common.Symbol("id")},
					},
				},
				[]int{0},
			},
			true,
		},
		{
			"prefix",
			"(prefix (rnrs base) id)",
			importSpecResolution{
				importSetResolution{
					identifierSpecPrefix{
						identifierSpecAll{},
						common.Symbol("id"),
					},
				},
				[]int{0},
			},
			true,
		},
		{
			"rename",
			"(rename (rnrs base) (id thing))",
			importSpecResolution{
				importSetResolution{
					identifierSpecRename{
						identifierSpecAll{},
						[]identifierBinding{
							identifierBinding{
								common.Symbol("thing"),
								common.Symbol("id"),
							},
						},
					},
				},
				[]int{0},
			},
			true,
		},
		{
			"empty version",
			"(rnrs base ())",
			importSpecResolution{
				importSetResolution{
					identifierSpecAll{},
				},
				[]int{0},
			},
			true,
		},
		{
			"partial version",
			"(rnrs base (6))",
			importSpecResolution{
				importSetResolution{
					identifierSpecAll{},
				},
				[]int{0},
			},
			true,
		},
		{
			"partial wrong version",
			"(rnrs base (5))",
			importSpecResolution{},
			false,
		},
		{
			"full version",
			"(rnrs base (6 4))",
			importSpecResolution{
				importSetResolution{
					identifierSpecAll{},
				},
				[]int{0},
			},
			true,
		},
		{
			"full wrong version",
			"(rnrs base (6 3))",
			importSpecResolution{},
			false,
		},
		{
			"and version",
			"(rnrs base (and (6) ((>= 5))))",
			importSpecResolution{
				importSetResolution{
					identifierSpecAll{},
				},
				[]int{0},
			},
			true,
		},
		{
			"wrong and version",
			"(rnrs base (and (6) ((<= 5))))",
			importSpecResolution{},
			false,
		},
		{
			"or version",
			"(rnrs base (or (6) (4)))",
			importSpecResolution{
				importSetResolution{
					identifierSpecAll{},
				},
				[]int{0},
			},
			true,
		},
		{
			"wrong or version",
			"(rnrs base (or (5) (4)))",
			importSpecResolution{},
			false,
		},
		{
			"not version",
			"(rnrs base (not (4)))",
			importSpecResolution{
				importSetResolution{
					identifierSpecAll{},
				},
				[]int{0},
			},
			true,
		},
		{
			"wrong not version",
			"(rnrs base (not (6)))",
			importSpecResolution{},
			false,
		},
		{
			">= sub-version",
			"(rnrs base ((>= 4)))",
			importSpecResolution{
				importSetResolution{
					identifierSpecAll{},
				},
				[]int{0},
			},
			true,
		},
		{
			"wrong >= sub-version",
			"(rnrs base ((>= 7)))",
			importSpecResolution{},
			false,
		},
		{
			"<= sub-version",
			"(rnrs base ((<= 7)))",
			importSpecResolution{
				importSetResolution{
					identifierSpecAll{},
				},
				[]int{0},
			},
			true,
		},
		{
			"wrong <= sub-version",
			"(rnrs base ((<= 4)))",
			importSpecResolution{},
			false,
		},
		{
			"and sub-version",
			"(rnrs base ((and 6 (>= 4))))",
			importSpecResolution{
				importSetResolution{
					identifierSpecAll{},
				},
				[]int{0},
			},
			true,
		},
		{
			"wrong and sub-version",
			"(rnrs base ((and 6 (<= 4))))",
			importSpecResolution{},
			false,
		},
		{
			"or sub-version",
			"(rnrs base ((or 6 4)))",
			importSpecResolution{
				importSetResolution{
					identifierSpecAll{},
				},
				[]int{0},
			},
			true,
		},
		{
			"wrong or sub-version",
			"(rnrs base ((or 5 4)))",
			importSpecResolution{},
			false,
		},
		{
			"not sub-version",
			"(rnrs base ((not 4)))",
			importSpecResolution{
				importSetResolution{
					identifierSpecAll{},
				},
				[]int{0},
			},
			true,
		},
		{
			"wrong not sub-version",
			"(rnrs base ((not 6)))",
			importSpecResolution{},
			false,
		},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			data, err := read.ReadString(test.source)
			if err != nil {
				t.Fatal(err)
			} else if len(data) != 1 {
				t.Fatalf("encountered %v data in pattern", len(data))
			}
			spec, err := newImportSpec(common.NewWrappedSyntax(data[0]))
			if err != nil {
				t.Fatal(err)
			}
			res, ok := spec.resolve(library)
			if ok != test.ok {
				t.Fatalf("\nexpected ok: %v\n     got ok: %v\n", test.ok, ok)
			}
			if ok && !reflect.DeepEqual(res, test.resolution) {
				t.Fatalf("\nexpected: %#v\n     got: %#v", test.resolution, res)
			}
		})
	}
}
