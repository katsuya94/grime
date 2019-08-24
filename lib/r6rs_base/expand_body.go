package r6rs_base

import (
	"fmt"

	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/lib/r6rs"
)

func bodyTransformer(name string) *BaseTransformer {
	return &BaseTransformer{func(ctx r6rs.ExpansionContext, syntax common.Syntax, mark *common.M) (r6rs.CoreForm, error) {
		return nil, fmt.Errorf("%v in expression context", name)
	}}
}

var (
	beginTransformer        = &BaseTransformer{transformBegin}
	defineSyntaxTransformer = bodyTransformer("define-syntax")
)

var bodyTransformers = []*BaseTransformer{
	beginTransformer,
	defineSyntaxTransformer,
}

func expandBody(ctx r6rs.ExpansionContext, forms []common.Syntax, mark *common.M) (r6rs.CoreForm, error) {
	for i := 0; i < len(forms); i++ {
		form := forms[i]
		transformer, ok, err := partiallyExpand(ctx.Expander, form, ctx.Env)
		if err != nil {
			return nil, err
		} else if !ok {
			break
		}
		switch transformer {
		case beginTransformer:
		case defineSyntaxTransformer:
		default:
			panic("unknown body transformer")
		}
	}
	output := common.Pair{r6rs.SequenceId.Mark(mark).WrappedSyntax, list(syntaxDatumSlice(forms)...)}
	return ctx.Expand(common.NewSyntax(output))
}

func partiallyExpand(expander r6rs.Expander, syntax common.Syntax, env common.Environment) (common.Procedure, bool, error) {
	ctx := r6rs.ExpansionContext{Expander: expander, Env: env}
	mark := common.NewMark()
	switch transformer := matchTransformer(syntax, env).(type) {
	case r6rs.CoreTransformer:
		return r6rs.ExpandCoreTransformer(ctx, transformer, syntax, mark)
	case *BaseTransformer:
		return ExpandBaseTransformer(ctx, transformer, syntax, mark)
	default:
		if transformer == nil {
			if _, ok := patternApplication.Match(syntax); ok {
				return ExpandBaseTransformer(ctx, &BaseTransformer{transformApplication}, syntax, mark)
			}
			if _, ok := syntax.Identifier(); ok {
				return ExpandBaseTransformer(ctx, &BaseTransformer{transformId}, syntax, mark)
			}
			return ExpandBaseTransformer(ctx, &BaseTransformer{transformLiteral}, syntax, mark)
		}
		syntax, err := applyTransformer(transformer, syntax, mark)
		if err != nil {
			return nil, err
		}
		return ctx.Expand(syntax)
	}
}
