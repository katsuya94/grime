package r6rs

import "github.com/katsuya94/grime/common"

type CpsTransformContext struct {
	indices map[common.Binding]int
}

func NewCpsTransformContext() CpsTransformContext {
	return CpsTransformContext{map[common.Binding]int{}}
}

func (ctx CpsTransformContext) Add(binding common.Binding) {
	i := len(ctx.indices)
	ctx.indices[binding] = i
}

func (ctx CpsTransformContext) Index(binding common.Binding) int {
	i, ok := ctx.indices[binding]
	if !ok {
		panic("binding not in context")
	}
	return i
}

func CpsTransform(coreForm CoreForm) common.Expression {
	ctx := NewCpsTransformContext()
	return coreForm.CpsTransform(ctx)
}
