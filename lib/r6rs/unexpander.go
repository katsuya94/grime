package r6rs

import (
	"github.com/katsuya94/grime/common"
)

type Unexpander interface {
	common.CoreForm
	Unexpand() common.Syntax
}
