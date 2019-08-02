package r6rs

import (
	"fmt"

	"github.com/katsuya94/grime/common"
)

type Global struct {
	Id       common.Identifier
	Location common.Location
}

type CpsTransformContext struct {
	localIds []common.Identifier
	globals  []Global
}

func NewCpsTransformContext(globals []Global) CpsTransformContext {
	return CpsTransformContext{[]common.Identifier{}, globals}
}

func (ctx CpsTransformContext) New() CpsTransformContext {
	return CpsTransformContext{[]common.Identifier{}, ctx.globals}
}

func (ctx *CpsTransformContext) Add(id common.Identifier) {
	ctx.localIds = append(ctx.localIds, id)
}

func (ctx CpsTransformContext) Index(id common.Identifier) (int, error) {
	for i, localId := range ctx.localIds {
		if localId.BoundEqual(id) {
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

func (ctx CpsTransformContext) Top(id common.Identifier) (common.Location, error) {
	for _, global := range ctx.globals {
		if global.Id.BoundEqual(id) {
			return global.Location, nil
		}
	}
	return common.Location{}, fmt.Errorf("cps transform: %v not in context", id.Name())
}
