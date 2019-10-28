package runtime

import (
	"reflect"
	"testing"

	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/read"
)

func TestNewLibrary(t *testing.T) {
	tests := []struct {
		name     string
		source   string
		expected Library
		error    string
	}{
		{
			"empty library",
			"(library (name) (export) (import))",
			Library{
				name: []common.Symbol{common.Symbol("name")},
			},
			"",
		},
		{
			"empty library with multiple-identifier name",
			"(library (name id) (export) (import))",
			Library{
				name: []common.Symbol{common.Symbol("name"), common.Symbol("id")},
			},
			"",
		},
		{
			"empty library with empty version",
			"(library (name ()) (export) (import))",
			Library{
				name: []common.Symbol{common.Symbol("name")},
			},
			"",
		},
		{
			"empty library with single-number version",
			"(library (name (1)) (export) (import))",
			Library{
				name:    []common.Symbol{common.Symbol("name")},
				version: []subVersion{1},
			},
			"",
		},
		{
			"empty library with multiple-number version",
			"(library (name (1 0)) (export) (import))",
			Library{
				name:    []common.Symbol{common.Symbol("name")},
				version: []subVersion{1, 0},
			},
			"",
		},
		{
			"non-empty library",
			"(library (name) (export) (import) id)",
			Library{
				name: []common.Symbol{common.Symbol("name")},
				body: []common.Syntax{
					common.NewSyntax(
						common.NewWrappedSyntax(
							common.Symbol("id"),
							&common.SourceLocationTree{
								common.SourceLocation{
									File:   "string",
									Line:   0,
									Column: 34,
									Offset: 34,
									Length: 2,
								},
								nil,
							},
						),
					),
				},
			},
			"",
		},
		{
			"empty library with direct export",
			"(library (name) (export id) (import))",
			Library{
				name: []common.Symbol{common.Symbol("name")},
				exportSpecs: []identifierBinding{
					{common.Symbol("id"), common.Symbol("id")},
				},
			},
			"",
		},
		{
			"empty library with renamed export",
			"(library (name) (export (rename (id name))) (import))",
			Library{
				name: []common.Symbol{common.Symbol("name")},
				exportSpecs: []identifierBinding{
					{common.Symbol("id"), common.Symbol("name")},
				},
			},
			"",
		},
		{
			"empty library with multiple renamed exports",
			"(library (name) (export (rename (id name) (foo bar))) (import))",
			Library{
				name: []common.Symbol{common.Symbol("name")},
				exportSpecs: []identifierBinding{
					{common.Symbol("id"), common.Symbol("name")},
					{common.Symbol("foo"), common.Symbol("bar")},
				},
			},
			"",
		},
		{
			"empty library with renamed export and direct export",
			"(library (name) (export (rename (id name)) foo) (import))",
			Library{
				name: []common.Symbol{common.Symbol("name")},
				exportSpecs: []identifierBinding{
					{common.Symbol("id"), common.Symbol("name")},
					{common.Symbol("foo"), common.Symbol("foo")},
				},
			},
			"",
		},
		{
			"empty library with import",
			"(library (name) (export) (import (rnrs)))",
			Library{
				name: []common.Symbol{common.Symbol("name")},
				importSpecs: []importSpec{
					{
						importSetLibraryReference{
							[]common.Symbol{common.Symbol("rnrs")},
							versionReferenceSubVersionReferences{},
						},
						[]int{0},
					},
				},
			},
			"",
		},
		{
			"empty library with multi-identifier import",
			"(library (name) (export) (import (rnrs base)))",
			Library{
				name: []common.Symbol{common.Symbol("name")},
				importSpecs: []importSpec{
					{
						importSetLibraryReference{
							[]common.Symbol{common.Symbol("rnrs"), common.Symbol("base")},
							versionReferenceSubVersionReferences{},
						},
						[]int{0},
					},
				},
			},
			"",
		},
		{
			"empty library with single-number version import",
			"(library (name) (export) (import (rnrs (6))))",
			Library{
				name: []common.Symbol{common.Symbol("name")},
				importSpecs: []importSpec{
					{
						importSetLibraryReference{
							[]common.Symbol{common.Symbol("rnrs")},
							versionReferenceSubVersionReferences{
								[]subVersionReference{
									subVersion(6),
								},
							},
						},
						[]int{0},
					},
				},
			},
			"",
		},
		{
			"empty library with multiple-number version import",
			"(library (name) (export) (import (rnrs (6 0))))",
			Library{
				name: []common.Symbol{common.Symbol("name")},
				importSpecs: []importSpec{
					{
						importSetLibraryReference{
							[]common.Symbol{common.Symbol("rnrs")},
							versionReferenceSubVersionReferences{
								[]subVersionReference{
									subVersion(6),
									subVersion(0),
								},
							},
						},
						[]int{0},
					},
				},
			},
			"",
		},
		{
			"empty library with and version import",
			"(library (name) (export) (import (rnrs (and (6) (7)))))",
			Library{
				name: []common.Symbol{common.Symbol("name")},
				importSpecs: []importSpec{
					{
						importSetLibraryReference{
							[]common.Symbol{common.Symbol("rnrs")},
							versionReferenceAnd{
								[]versionReference{
									versionReferenceSubVersionReferences{
										[]subVersionReference{subVersion(6)},
									},
									versionReferenceSubVersionReferences{
										[]subVersionReference{subVersion(7)},
									},
								},
							},
						},
						[]int{0},
					},
				},
			},
			"",
		},
		{
			"empty library with or version import",
			"(library (name) (export) (import (rnrs (or (6) (7)))))",
			Library{
				name: []common.Symbol{common.Symbol("name")},
				importSpecs: []importSpec{
					{
						importSetLibraryReference{
							[]common.Symbol{common.Symbol("rnrs")},
							versionReferenceOr{
								[]versionReference{
									versionReferenceSubVersionReferences{
										[]subVersionReference{subVersion(6)},
									},
									versionReferenceSubVersionReferences{
										[]subVersionReference{subVersion(7)},
									},
								},
							},
						},
						[]int{0},
					},
				},
			},
			"",
		},
		{
			"empty library with not version import",
			"(library (name) (export) (import (rnrs (not (6)))))",
			Library{
				name: []common.Symbol{common.Symbol("name")},
				importSpecs: []importSpec{
					{
						importSetLibraryReference{
							[]common.Symbol{common.Symbol("rnrs")},
							versionReferenceNot{
								versionReferenceSubVersionReferences{
									[]subVersionReference{subVersion(6)},
								},
							},
						},
						[]int{0},
					},
				},
			},
			"",
		},
		{
			"empty library with gte version import",
			"(library (name) (export) (import (rnrs ((>= 6)))))",
			Library{
				name: []common.Symbol{common.Symbol("name")},
				importSpecs: []importSpec{
					{
						importSetLibraryReference{
							[]common.Symbol{common.Symbol("rnrs")},
							versionReferenceSubVersionReferences{
								[]subVersionReference{
									subVersionReferenceGte{subVersion(6)},
								},
							},
						},
						[]int{0},
					},
				},
			},
			"",
		},
		{
			"empty library with lte version import",
			"(library (name) (export) (import (rnrs ((<= 6)))))",
			Library{
				name: []common.Symbol{common.Symbol("name")},
				importSpecs: []importSpec{
					{
						importSetLibraryReference{
							[]common.Symbol{common.Symbol("rnrs")},
							versionReferenceSubVersionReferences{
								[]subVersionReference{
									subVersionReferenceLte{subVersion(6)},
								},
							},
						},
						[]int{0},
					},
				},
			},
			"",
		},
		{
			"empty library with sub-version and version import",
			"(library (name) (export) (import (rnrs ((and 6 7)))))",
			Library{
				name: []common.Symbol{common.Symbol("name")},
				importSpecs: []importSpec{
					{
						importSetLibraryReference{
							[]common.Symbol{common.Symbol("rnrs")},
							versionReferenceSubVersionReferences{
								[]subVersionReference{
									subVersionReferenceAnd{
										[]subVersionReference{
											subVersion(6),
											subVersion(7),
										},
									},
								},
							},
						},
						[]int{0},
					},
				},
			},
			"",
		},
		{
			"empty library with sub-version or version import",
			"(library (name) (export) (import (rnrs ((or 6 7)))))",
			Library{
				name: []common.Symbol{common.Symbol("name")},
				importSpecs: []importSpec{
					{
						importSetLibraryReference{
							[]common.Symbol{common.Symbol("rnrs")},
							versionReferenceSubVersionReferences{
								[]subVersionReference{
									subVersionReferenceOr{
										[]subVersionReference{
											subVersion(6),
											subVersion(7),
										},
									},
								},
							},
						},
						[]int{0},
					},
				},
			},
			"",
		},
		{
			"empty library with sub-version not version import",
			"(library (name) (export) (import (rnrs ((not 6)))))",
			Library{
				name: []common.Symbol{common.Symbol("name")},
				importSpecs: []importSpec{
					{
						importSetLibraryReference{
							[]common.Symbol{common.Symbol("rnrs")},
							versionReferenceSubVersionReferences{
								[]subVersionReference{
									subVersionReferenceNot{
										subVersion(6),
									},
								},
							},
						},
						[]int{0},
					},
				},
			},
			"",
		},
		{
			"empty library with meta import",
			"(library (name) (export) (import (for (rnrs) (meta 3))))",
			Library{
				name: []common.Symbol{common.Symbol("name")},
				importSpecs: []importSpec{
					{
						importSetLibraryReference{
							[]common.Symbol{common.Symbol("rnrs")},
							versionReferenceSubVersionReferences{},
						},
						[]int{3},
					},
				},
			},
			"",
		},
		{
			"empty library with run import",
			"(library (name) (export) (import (for (rnrs) run)))",
			Library{
				name: []common.Symbol{common.Symbol("name")},
				importSpecs: []importSpec{
					{
						importSetLibraryReference{
							[]common.Symbol{common.Symbol("rnrs")},
							versionReferenceSubVersionReferences{},
						},
						[]int{0},
					},
				},
			},
			"",
		},
		{
			"empty library with expand import",
			"(library (name) (export) (import (for (rnrs) expand)))",
			Library{
				name: []common.Symbol{common.Symbol("name")},
				importSpecs: []importSpec{
					{
						importSetLibraryReference{
							[]common.Symbol{common.Symbol("rnrs")},
							versionReferenceSubVersionReferences{},
						},
						[]int{1},
					},
				},
			},
			"",
		},
		{
			"empty library with multiple imports",
			"(library (name) (export) (import (for (rnrs) run expand)))",
			Library{
				name: []common.Symbol{common.Symbol("name")},
				importSpecs: []importSpec{
					{
						importSetLibraryReference{
							[]common.Symbol{common.Symbol("rnrs")},
							versionReferenceSubVersionReferences{},
						},
						[]int{0, 1},
					},
				},
			},
			"",
		},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			syntax := read.MustReadSyntax(test.source)
			actual, err := NewLibrary(syntax)
			if test.error != "" {
				if err == nil || err.Error() != test.error {
					t.Fatalf("\nexpected error: %v\n     got error: %v\n", test.error, err)
				}
			} else if err != nil {
				t.Fatal(err)
			} else {
				null := syntax
				for null.Unwrap() != common.Null {
					pair, _ := null.Pair()
					null = common.NewSyntax(pair.Rest)
				}
				test.expected.nullSourceLocationTree = null.SourceLocationTree()
				if !reflect.DeepEqual(actual, test.expected) {
					t.Errorf("\nexpected: %#v\n     got: %#v", test.expected, actual)
				}
			}
		})
	}
}
