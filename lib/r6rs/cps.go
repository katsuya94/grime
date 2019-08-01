package r6rs

import (
	"fmt"

	"github.com/katsuya94/grime/common"
)

type CpsTransformContext struct {
	ids []common.Identifier
}

func NewCpsTransformContext() CpsTransformContext {
	return CpsTransformContext{[]common.Identifier{}}
}

func (ctx *CpsTransformContext) Add(id common.Identifier) {
	ctx.ids = append(ctx.ids, id)
}

func (ctx CpsTransformContext) Index(id common.Identifier) (int, error) {
	for i, candidate := range ctx.ids {
		if candidate.BoundEqual(id) {
			return i, nil
		}
	}
	return -1, fmt.Errorf("cps transform: %v not in context", id.Name())
}

func (ctx CpsTransformContext) IndexOrDie(id common.Identifier) int {
	i, err := ctx.Index(id)
	if err != nil {
		panic(err.Error())
	}
	return i
}

func CpsTransform(coreForm CoreForm) (common.Expression, error) {
	ctx := NewCpsTransformContext()
	return coreForm.CpsTransform(ctx)
}
