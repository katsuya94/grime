package util

import "github.com/katsuya94/grime/common"

func Invoke(p common.Procedure, args ...common.Datum) (common.Datum, error) {
	return common.WithEscape(func(escape common.Continuation) (common.Evaluation, error) {
		return p.Call(escape, args...)
	})
}
