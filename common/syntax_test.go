package common_test

import (
	"testing"

	"github.com/katsuya94/grime/common"
	. "github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/read"
	"github.com/stretchr/testify/require"
)

func data(source string) []common.Datum {
	return read.MustReadData(source)
}

func datum(source string) common.Datum {
	return read.MustReadDatum(source)
}

func wrap(datum common.Datum) common.WrappedSyntax {
	return common.NewWrappedSyntax(datum, nil)
}

func TestIsSyntaxWrappedSyntax(t *testing.T) {
	require.True(t, IsSyntax(wrap(datum("thing"))))
}
func TestIsSyntaxPairBothSyntax(t *testing.T) {
	require.True(t, IsSyntax(Pair{wrap(datum("thing")), wrap(datum("stuff"))}))
}

func TestIsSyntaxPairFirstNotSyntax(t *testing.T) {
	require.False(t, IsSyntax(Pair{datum("thing"), wrap(datum("stuff"))}))
}

func TestIsSyntaxPairRestNotSyntax(t *testing.T) {
	require.False(t, IsSyntax(Pair{wrap(datum("thing")), datum("stuff")}))
}

func TestIsSyntaxPairBothNotSyntax(t *testing.T) {
	require.False(t, IsSyntax(Pair{datum("thing"), datum("stuff")}))
}

func TestWrappedSyntax_IsIdentifierSymbol(t *testing.T) {
	require.True(t, wrap(datum("thing")).IsIdentifier())
}

func TestWrappedSyntax_IsIdentifierNonSymbol(t *testing.T) {
	require.False(t, wrap(datum("#f")).IsIdentifier())
}

// TODO: consider using alias types to get signature syntax.Identifier().Location()
