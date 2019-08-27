package r6rs_base

import (
	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/lib/r6rs"
	"github.com/katsuya94/grime/read"
)

var (
	patternMacroUseSet                 = common.MustCompileSimplePatternWithIdentifierLiterals(read.MustReadDatum("(set! keyword _)"), setId)
	patternMacroUseList                = common.MustCompileSimplePattern(read.MustReadDatum("(keyword _ ...)"))
	patternMacroUseImproperList        = common.MustCompileSimplePattern(read.MustReadDatum("(keyword _ ... . _)"))
	patternMacroUseSingletonIdentifier = common.MustCompileSimplePattern(read.MustReadDatum("keyword"))
)

func Expand(syntax common.Syntax, env common.Environment) (common.CoreForm, error) {
	return baseExpander{env}.Expand(syntax, env)
}

type baseExpander struct {
	globalEnv common.Environment
}

func (e baseExpander) globalCtx() common.ExpansionContext {
	return common.ExpansionContext{Expander: e, Env: e.globalEnv}
}

func (e baseExpander) Expand(syntax common.Syntax, env common.Environment) (common.CoreForm, error) {
	ctx := common.ExpansionContext{Expander: e, Env: env}
	mark := common.NewMark()
	switch transformer := matchTransformer(syntax, env).(type) {
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
		return ctx.Expand(syntax)
	}
}

func matchTransformer(syntax common.Syntax, env common.Environment) common.Procedure {
	var transformer common.Procedure
	transformer = r6rs.MatchTransformer(syntax, env, patternMacroUseSet)
	if transformer != nil {
		return transformer
	}
	transformer = r6rs.MatchTransformer(syntax, env, patternMacroUseList)
	if transformer != nil {
		return transformer
	}
	transformer = r6rs.MatchTransformer(syntax, env, patternMacroUseImproperList)
	if transformer != nil {
		return transformer
	}
	transformer = r6rs.MatchTransformer(syntax, env, patternMacroUseSingletonIdentifier)
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
