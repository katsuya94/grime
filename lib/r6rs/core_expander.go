package r6rs

import (
	"fmt"

	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/read"
)

type CoreTransformer interface {
	common.Procedure
	Transform(common.ExpansionContext, common.Syntax, *common.M) (common.CoreForm, error)
}

func NewCoreTransformer(transform func(common.ExpansionContext, common.Syntax, *common.M) (common.CoreForm, error)) CoreTransformer {
	return coreTransformerImpl{transform}
}

type coreTransformerImpl struct {
	transform func(common.ExpansionContext, common.Syntax, *common.M) (common.CoreForm, error)
}

func (coreTransformerImpl) Call(common.Continuation, ...common.Datum) (common.Evaluation, error) {
	return common.ErrorC(fmt.Errorf("core transformer called"))
}

func (t coreTransformerImpl) Transform(ctx common.ExpansionContext, syntax common.Syntax, mark *common.M) (common.CoreForm, error) {
	return t.transform(ctx, syntax, mark)
}

var patternMacroUseList = common.MustCompileSimplePattern(read.MustReadDatum("(keyword _ ...)"))

type coreExpander struct {
	newMark func() *common.M
}

func Expand(syntax common.Syntax) (common.CoreForm, error) {
	return coreExpander{func() *common.M {
		return common.NewMark()
	}}.Expand(syntax, CoreEnvironment)
}

func ExpandWithMarks(syntax common.Syntax, marks []common.M) (common.CoreForm, error) {
	i := 0
	return coreExpander{func() *common.M {
		m := &marks[i]
		i++
		return m
	}}.Expand(syntax, CoreEnvironment)
}

func (e coreExpander) Expand(syntax common.Syntax, env common.Environment) (common.CoreForm, error) {
	switch transformer := MatchTransformer(syntax, env, patternMacroUseList).(type) {
	case CoreTransformer:
		ctx := common.ExpansionContext{Expander: e, Env: env}
		// TODO: marks should record information about their introducing macro
		mark := e.newMark()
		return ExpandCoreTransformer(ctx, transformer, syntax, mark)
	default:
		if transformer == nil {
			return nil, fmt.Errorf("expand: unhandled syntax %v at %v", common.Write(syntax.Datum()), syntax.SourceLocation())
		}
		return nil, fmt.Errorf("expand: encountered non-core transformer %v", common.Write(transformer))
	}
}

func ExpandCoreTransformer(ctx common.ExpansionContext, transformer CoreTransformer, syntax common.Syntax, mark *common.M) (common.CoreForm, error) {
	return transformer.Transform(ctx, syntax, mark)
}

func MatchTransformer(syntax common.Syntax, env common.Environment, pattern common.SimplePattern) common.Procedure {
	result, ok := pattern.Match(syntax)
	if !ok {
		return nil
	}
	id, ok := result[common.Symbol("keyword")].(common.Syntax).Identifier()
	if !ok {
		return nil
	}
	binding := id.Binding()
	if binding == nil {
		return nil
	}
	role := env.Lookup(binding)
	if role == nil {
		return nil
	}
	syntacticAbstraction, ok := role.(common.SyntacticAbstraction)
	if !ok {
		return nil
	}
	return syntacticAbstraction.Transformer
}
