package common_test

import (
	"testing"

	. "github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/read"
	"github.com/katsuya94/grime/test"
	"github.com/stretchr/testify/require"
)

func TestIsSyntaxWrappedSyntax(t *testing.T) {
	require.True(t, IsSyntax(test.Syntax("thing").Datum()))
}
func TestIsSyntaxPairBothSyntax(t *testing.T) {
	require.True(t, IsSyntax(Pair{test.Syntax("thing").Datum(), test.Syntax("stuff").Datum()}))
}

func TestIsSyntaxPairFirstNotSyntax(t *testing.T) {
	require.False(t, IsSyntax(Pair{read.MustReadDatum("thing"), test.Syntax("stuff")}))
}

func TestIsSyntaxPairRestNotSyntax(t *testing.T) {
	require.False(t, IsSyntax(Pair{test.Syntax("thing"), read.MustReadDatum("stuff")}))
}

func TestIsSyntaxPairBothNotSyntax(t *testing.T) {
	require.False(t, IsSyntax(Pair{read.MustReadDatum("thing"), read.MustReadDatum("stuff")}))
}
