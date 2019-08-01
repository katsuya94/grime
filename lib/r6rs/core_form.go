package r6rs

import (
	"github.com/katsuya94/grime/common"
)

type CoreForm interface {
	Unexpand() common.Syntax
	CpsTransform(CpsTransformContext) (common.Expression, error)
}
