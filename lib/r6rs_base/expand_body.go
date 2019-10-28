package r6rs_base

import (
	"fmt"

	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/lib/r6rs"
	"github.com/katsuya94/grime/read"
)

type bodyExpansionContext struct {
	common.ExpansionContext
	Rest []common.Syntax
}

type bodyTransformer interface {
	r6rs.CoreTransformer
	transformBody(bodyExpansionContext, common.Syntax, *common.M, *common.Scope) (common.CoreForm, common.Environment, error)
}

type bodyTransformerImpl struct {
	r6rs.CoreTransformer
	transformBodyFunc func(bodyExpansionContext, common.Syntax, *common.M, *common.Scope) (common.CoreForm, common.Environment, error)
}

func (t bodyTransformerImpl) transformBody(ctx bodyExpansionContext, syntax common.Syntax, mark *common.M, scope *common.Scope) (common.CoreForm, common.Environment, error) {
	return t.transformBodyFunc(ctx, syntax, mark, scope)
}

func newBodyTransformer(transformBody func(bodyExpansionContext, common.Syntax, *common.M, *common.Scope) (common.CoreForm, common.Environment, error)) bodyTransformer {
	return bodyTransformerImpl{
		r6rs.NewCoreTransformer(func(ctx common.ExpansionContext, syntax common.Syntax, mark *common.M) (common.CoreForm, error) {
			return nil, fmt.Errorf("body form in expression context: %v at %v", syntaxKeywordForErrMsg(syntax), syntax.SourceLocation())
		}),
		transformBody,
	}
}

func expandBody(ctx common.ExpansionContext, forms []common.Syntax, mark *common.M, scope *common.Scope) (common.CoreForm, common.Environment, error) {
	if len(forms) == 0 {
		return nil, nil, fmt.Errorf("unexpected final form")
	}
	transformer, form, err := partiallyExpand(ctx, forms[0])
	if err != nil {
		return nil, nil, err
	}
	if transformer, ok := transformer.(bodyTransformer); ok {
		ctx := bodyExpansionContext{ctx, forms[1:]}
		return transformer.transformBody(ctx, form, mark, scope)
	}
	forms = append([]common.Syntax{form}, forms[1:]...)
	output := common.Pair{r6rs.SequenceId.Mark(mark).WrappedSyntax, list(syntaxDatumSlice(forms)...)}
	coreForm, err := ctx.Expander.Expand(ctx, common.NewSyntax(output))
	if err != nil {
		return nil, nil, err
	}
	return coreForm, ctx.Env, nil
}

func partiallyExpand(ctx common.ExpansionContext, syntax common.Syntax) (common.Procedure, common.Syntax, error) {
	mark := common.NewMark()
	switch transformer := matchTransformer(ctx.Env, syntax).(type) {
	case r6rs.CoreTransformer:
		return transformer, syntax, nil
	default:
		if transformer == nil {
			return nil, syntax, nil
		}
		syntax, err := applyTransformer(transformer, syntax, mark)
		if err != nil {
			return nil, common.Syntax{}, err
		}
		return partiallyExpand(ctx, syntax)
	}
}

var beginTransformer = bodyTransformerImpl{r6rs.NewCoreTransformer(transformBegin), transformBodyBegin}
var patternBegin = common.MustCompileSimplePattern(read.MustReadDatum("(begin body ...)"))

func transformBegin(ctx common.ExpansionContext, syntax common.Syntax, mark *common.M) (common.CoreForm, error) {
	result, ok := patternBegin.Match(syntax)
	if !ok {
		return nil, fmt.Errorf("%v: bad syntax", syntaxKeywordForErrMsg(syntax))
	}
	scope := common.NewScope()
	forms := make([]common.Syntax, len(result[common.Symbol("body")].([]interface{})))
	for i, syntax := range result[common.Symbol("body")].([]interface{}) {
		forms[i] = syntax.(common.Syntax).Push(scope)
	}
	coreForm, _, err := expandBody(ctx, forms, mark, scope)
	if err != nil {
		return nil, err
	}
	return coreForm, nil
}

func transformBodyBegin(ctx bodyExpansionContext, syntax common.Syntax, mark *common.M, scope *common.Scope) (common.CoreForm, common.Environment, error) {
	result, ok := patternBegin.Match(syntax)
	if !ok {
		return nil, nil, fmt.Errorf("%v: bad syntax", syntaxKeywordForErrMsg(syntax))
	}
	n := len(result[common.Symbol("body")].([]interface{}))
	forms := make([]common.Syntax, n, n+len(ctx.Rest))
	for i, syntax := range result[common.Symbol("body")].([]interface{}) {
		forms[i] = syntax.(common.Syntax)
	}
	forms = append(forms, ctx.Rest...)
	return expandBody(ctx.ExpansionContext, forms, mark, scope)
}

var defineSyntaxTransformer = newBodyTransformer(transformBodyDefineSyntax)
var patternDefineSyntax = common.MustCompileSimplePattern(read.MustReadDatum("(define-syntax id transformer)"))

func transformBodyDefineSyntax(ctx bodyExpansionContext, syntax common.Syntax, mark *common.M, scope *common.Scope) (common.CoreForm, common.Environment, error) {
	result, ok := patternDefineSyntax.Match(syntax)
	if !ok {
		return nil, nil, fmt.Errorf("%v: bad syntax", syntaxKeywordForErrMsg(syntax))
	}
	id, ok := result[common.Symbol("id")].(common.Syntax).Identifier()
	if !ok {
		return nil, nil, fmt.Errorf("%v: bad syntax", syntaxKeywordForErrMsg(syntax))
	}
	_, binding := common.Bind(id, scope)
	syntax = result[common.Symbol("transformer")].(common.Syntax)
	rhsCtx := ctx.Expander.(Expander).Context(ctx.Phase + 1)
	coreForm, err := rhsCtx.Expander.Expand(rhsCtx, syntax)
	if err != nil {
		return nil, nil, err
	}
	expression, err := coreForm.CpsTransform(common.NewCpsTransformContext([]common.Global{}))
	if err != nil {
		return nil, nil, err
	}
	datum, err := common.Evaluate(common.NewEvaluationContext(), expression)
	if err != nil {
		return nil, nil, err
	}
	transformer, ok := datum.(common.Procedure)
	if !ok {
		return nil, nil, fmt.Errorf("%v: transformer is not a procedure", syntaxKeywordForErrMsg(syntax))
	}
	(&ctx.Env).Extend(binding, common.NewSyntacticAbstraction(transformer))
	return expandBody(ctx.ExpansionContext, ctx.Rest, mark, scope)
}
