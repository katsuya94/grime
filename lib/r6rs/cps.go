package r6rs

import "github.com/katsuya94/grime/common"

func CpsTransform(coreForm CoreForm) common.Expression {
	return coreForm.CpsTransform()
}
