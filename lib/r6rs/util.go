package r6rs

import (
	"github.com/katsuya94/grime/common"
)

func introduce(datum common.Datum) common.Datum {
	return Introduce(common.NewSyntax(common.NewWrappedSyntax(datum, nil))).Datum()
}

func list(data ...common.Datum) common.Datum {
	if len(data) == 0 {
		return introduce(common.Null)
	} else {
		return common.Pair{data[0], list(data[1:]...)}
	}
}
