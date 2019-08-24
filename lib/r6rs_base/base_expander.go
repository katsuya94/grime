package r6rs_base

import (
	"fmt"

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

type BaseTransformer interface {
	common.Procedure
	Transform(r6rs.ExpansionContext, common.Syntax, *common.M) (r6rs.CoreForm, error)
}

type baseTransformerImpl struct {
	transform func(r6rs.ExpansionContext, common.Syntax, *common.M) (r6rs.CoreForm, error)
}

func (baseTransformerImpl) Call(common.Continuation, ...common.Datum) (common.Evaluation, error) {
	return common.ErrorC(fmt.Errorf("base transformer called"))
}

func (t baseTransformerImpl) Transform(ctx r6rs.ExpansionContext, syntax common.Syntax, mark *common.M) (r6rs.CoreForm, error) {
	return t.transform(ctx, syntax, mark)
}

func newBaseTransformer(transform func(r6rs.ExpansionContext, common.Syntax, *common.M) (r6rs.CoreForm, error)) BaseTransformer {
	return baseTransformerImpl{transform}
}

type BaseExpander struct{}

func (e BaseExpander) Expand(syntax common.Syntax, env common.Environment) (r6rs.CoreForm, error) {
	ctx := r6rs.ExpansionContext{Expander: e, Env: env}
	mark := common.NewMark()
	switch transformer := matchTransformer(syntax, env).(type) {
	case r6rs.CoreTransformer:
		return r6rs.ExpandCoreTransformer(ctx, transformer, syntax, mark)
	case BaseTransformer:
		return ExpandBaseTransformer(ctx, transformer, syntax, mark)
	default:
		if transformer == nil {
			if _, ok := patternApplication.Match(syntax); ok {
				return ExpandBaseTransformer(ctx, applicationTransformer, syntax, mark)
			}
			if _, ok := syntax.Identifier(); ok {
				return ExpandBaseTransformer(ctx, idTransformer, syntax, mark)
			}
			return ExpandBaseTransformer(ctx, literalTransformer, syntax, mark)
		}
		syntax, err := applyTransformer(transformer, syntax, mark)
		if err != nil {
			return nil, err
		}
		return ctx.Expand(syntax)
	}
}

func ExpandBaseTransformer(ctx r6rs.ExpansionContext, transformer BaseTransformer, syntax common.Syntax, mark *common.M) (r6rs.CoreForm, error) {
	return transformer.Transform(ctx, syntax, mark)
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
