package test

import (
	"github.com/katsuya94/grime/common"
)

func Syntax(datum common.Datum) common.WrappedSyntax {
	return common.MustReadSyntax(datum, nil)
}
