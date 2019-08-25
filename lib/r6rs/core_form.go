package r6rs

import (
	"github.com/katsuya94/grime/common"
)

type CoreForm interface {
	CpsTransform(*CpsTransformContext) (common.Expression, error)
}

type Unexpander interface {
	CoreForm
	Unexpand() common.Syntax
}
