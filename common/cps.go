package common

import (
	"fmt"
)

type Global struct {
	Id       Identifier
	Location Location
}

type CpsTransformContext struct {
	localIds []Identifier
	globals  []Global
}

func NewCpsTransformContext(globals []Global) *CpsTransformContext {
	return &CpsTransformContext{[]Identifier{}, globals}
}

func (ctx CpsTransformContext) New() *CpsTransformContext {
	return &CpsTransformContext{[]Identifier{}, ctx.globals}
}

func (ctx *CpsTransformContext) Add(id Identifier) int {
	i := len(ctx.localIds)
	ctx.localIds = append(ctx.localIds, id)
	return i
}

func (ctx CpsTransformContext) Index(id Identifier) (int, error) {
	for i, localId := range ctx.localIds {
		if localId.BoundEqual(id) {
			return i, nil
		}
	}
	return -1, fmt.Errorf("cps transform: %v not in context", id.Name())
}

func (ctx CpsTransformContext) EvaluationContextTemplate() EvaluationContextTemplate {
	return EvaluationContextTemplate(len(ctx.localIds))
}

func (ctx CpsTransformContext) IndexOrDie(id Identifier) int {
	i, err := ctx.Index(id)
	if err != nil {
		panic(err.Error())
	}
	return i
}

func (ctx CpsTransformContext) Top(id Identifier) (Location, error) {
	for _, global := range ctx.globals {
		if global.Id.BoundEqual(id) {
			return global.Location, nil
		}
	}
	return Location{}, fmt.Errorf("cps transform: %v not in context", id.Name())
}
