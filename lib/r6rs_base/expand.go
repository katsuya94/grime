package r6rs_base

import (
	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/lib/r6rs"
)

func expand(ctx common.ExpansionContext, syntax common.Syntax) (common.CoreForm, error) {
	mark := common.NewMark()
	switch transformer := matchTransformer(ctx.Env, syntax).(type) {
	case r6rs.CoreTransformer:
		return r6rs.ExpandCoreTransformer(ctx, transformer, syntax, mark)
	default:
		if transformer == nil {
			if _, ok := patternApplication.Match(syntax); ok {
				return r6rs.ExpandCoreTransformer(ctx, applicationTransformer, syntax, mark)
			}
			if _, ok := syntax.Identifier(); ok {
				return r6rs.ExpandCoreTransformer(ctx, idTransformer, syntax, mark)
			}
			return r6rs.ExpandCoreTransformer(ctx, literalTransformer, syntax, mark)
		}
		syntax, err := applyTransformer(transformer, syntax, mark)
		if err != nil {
			return nil, err
		}
		return ctx.Expander.Expand(ctx, syntax)
	}
}

func matchTransformer(env common.Environment, syntax common.Syntax) common.Procedure {
	var transformer common.Procedure
	transformer = r6rs.MatchTransformer(env, syntax, patternMacroUseSet)
	if transformer != nil {
		return transformer
	}
	transformer = r6rs.MatchTransformer(env, syntax, patternMacroUseList)
	if transformer != nil {
		return transformer
	}
	transformer = r6rs.MatchTransformer(env, syntax, patternMacroUseImproperList)
	if transformer != nil {
		return transformer
	}
	transformer = r6rs.MatchTransformer(env, syntax, patternMacroUseSingletonIdentifier)
	if transformer != nil {
		return transformer
	}
	return nil
}

func applyTransformer(transformer common.Procedure, syntax common.Syntax, mark *common.M) (common.Syntax, error) {
	input := syntax.Mark(mark)
	output, err := common.WithEscape(func(escape common.Continuation) (common.Evaluation, error) {
		return transformer.Call(escape, input.Datum())
	})
	if err != nil {
		return common.Syntax{}, err
	}
	return common.NewSyntax(output).Mark(mark), nil
}
