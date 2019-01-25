package gen

import (
	"testing"

	"github.com/stretchr/testify/require"
)

func TestGoImportSetToken_AddLeaf(t *testing.T) {
	token := goImportSetToken{}
	token.add([]string{"fmt"})
	require.Equal(t, goImportSetToken{
		"fmt": &goImportSetSubtoken{[]string{}, nil},
	}, token)
}

func TestGoImportSetToken_AddLeafRest(t *testing.T) {
	token := goImportSetToken{}
	token.add([]string{"net", "http"})
	require.Equal(t, goImportSetToken{
		"http": &goImportSetSubtoken{[]string{"net"}, nil},
	}, token)
}

func TestGoImportSetToken_AddAdditionalLeaf(t *testing.T) {
	token := goImportSetToken{
		"fmt": &goImportSetSubtoken{[]string{}, nil},
	}
	token.add([]string{"io"})
	require.Equal(t, goImportSetToken{
		"fmt": &goImportSetSubtoken{[]string{}, nil},
		"io":  &goImportSetSubtoken{[]string{}, nil},
	}, token)
}

func TestGoImportSetToken_AddDuplicateLeaf(t *testing.T) {
	token := goImportSetToken{
		"io": &goImportSetSubtoken{[]string{}, nil},
	}
	token.add([]string{"io"})
	require.Equal(t, goImportSetToken{
		"io": &goImportSetSubtoken{[]string{}, nil},
	}, token)
}

func TestGoImportSetToken_AddExistingLeaf(t *testing.T) {
	token := goImportSetToken{
		"json": &goImportSetSubtoken{[]string{"encoding"}, nil},
	}
	token.add([]string{"decoding", "json"})
	require.Equal(t, goImportSetToken{
		"json": &goImportSetSubtoken{
			nil,
			goImportSetToken{
				"encoding": &goImportSetSubtoken{[]string{}, nil},
				"decoding": &goImportSetSubtoken{[]string{}, nil},
			},
		},
	}, token)
}

func TestGoImportSetToken_AddNestedLeaf(t *testing.T) {
	token := goImportSetToken{
		"json": &goImportSetSubtoken{
			nil,
			goImportSetToken{
				"encoding": &goImportSetSubtoken{[]string{}, nil},
				"decoding": &goImportSetSubtoken{[]string{}, nil},
			},
		},
	}
	token.add([]string{"recoding", "json"})
	require.Equal(t, goImportSetToken{
		"json": &goImportSetSubtoken{
			nil,
			goImportSetToken{
				"encoding": &goImportSetSubtoken{[]string{}, nil},
				"decoding": &goImportSetSubtoken{[]string{}, nil},
				"recoding": &goImportSetSubtoken{[]string{}, nil},
			},
		},
	}, token)
}

func TestGoImportSetToken_AddParentToNested(t *testing.T) {
	token := goImportSetToken{
		"json": &goImportSetSubtoken{
			nil,
			goImportSetToken{
				"encoding": &goImportSetSubtoken{[]string{}, nil},
				"decoding": &goImportSetSubtoken{[]string{}, nil},
			},
		},
	}
	token.add([]string{"json"})
	require.Equal(t, goImportSetToken{
		"json": &goImportSetSubtoken{
			nil,
			goImportSetToken{
				"":         nil,
				"encoding": &goImportSetSubtoken{[]string{}, nil},
				"decoding": &goImportSetSubtoken{[]string{}, nil},
			},
		},
	}, token)
}

func TestGoImportSetToken_AddNestedToParent(t *testing.T) {
	token := goImportSetToken{
		"json": &goImportSetSubtoken{[]string{}, nil},
	}
	token.add([]string{"encoding", "json"})
	require.Equal(t, goImportSetToken{
		"json": &goImportSetSubtoken{
			nil,
			goImportSetToken{
				"":         nil,
				"encoding": &goImportSetSubtoken{[]string{}, nil},
			},
		},
	}, token)
}
