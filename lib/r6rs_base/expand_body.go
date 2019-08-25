package r6rs_base

import (
	"fmt"

	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/lib/r6rs"
	"github.com/katsuya94/grime/read"
)

type BodyExpansionContext struct {
	r6rs.ExpansionContext
	Rest []common.Syntax
}

type BodyTransformer interface {
	r6rs.CoreTransformer
	TransformBody(BodyExpansionContext, common.Syntax, *common.M, *common.Scope) (r6rs.CoreForm, error)
}

type bodyTransformerImpl struct {
	r6rs.CoreTransformer
	transformBody func(BodyExpansionContext, common.Syntax, *common.M, *common.Scope) (r6rs.CoreForm, error)
}

func (t bodyTransformerImpl) TransformBody(ctx BodyExpansionContext, syntax common.Syntax, mark *common.M, scope *common.Scope) (r6rs.CoreForm, error) {
	return t.transformBody(ctx, syntax, mark, scope)
}

func newBodyTransformer(transformBody func(BodyExpansionContext, common.Syntax, *common.M, *common.Scope) (r6rs.CoreForm, error)) BodyTransformer {
	return bodyTransformerImpl{
		r6rs.NewCoreTransformer(func(ctx r6rs.ExpansionContext, syntax common.Syntax, mark *common.M) (r6rs.CoreForm, error) {
			return nil, fmt.Errorf("body form in expression context: %v at %v", syntaxKeywordForErrMsg(syntax), syntax.SourceLocation())
		}),
		transformBody,
	}
}

func expandBody(ctx r6rs.ExpansionContext, forms []common.Syntax, mark *common.M, scope *common.Scope) (r6rs.CoreForm, error) {
	if len(forms) == 0 {
		return nil, fmt.Errorf("unexpected final form")
	}
	transformer, form, err := partiallyExpand(ctx, forms[0])
	if err != nil {
		return nil, err
	}
	if transformer, ok := transformer.(BodyTransformer); ok {
		ctx := BodyExpansionContext{ctx, forms[1:]}
		return transformer.TransformBody(ctx, form, mark, scope)
	}
	forms = append([]common.Syntax{form}, forms[1:]...)
	output := common.Pair{r6rs.SequenceId.Mark(mark).WrappedSyntax, list(syntaxDatumSlice(forms)...)}
	return ctx.Expand(common.NewSyntax(output))
}

func partiallyExpand(ctx r6rs.ExpansionContext, syntax common.Syntax) (common.Procedure, common.Syntax, error) {
	mark := common.NewMark()
	switch transformer := matchTransformer(syntax, ctx.Env).(type) {
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

func transformBegin(ctx r6rs.ExpansionContext, syntax common.Syntax, mark *common.M) (r6rs.CoreForm, error) {
	result, ok := patternBegin.Match(syntax)
	if !ok {
		return nil, fmt.Errorf("%v: bad syntax", syntaxKeywordForErrMsg(syntax))
	}
	phase := result[common.Symbol("begin")].(common.Syntax).IdentifierOrDie().Phase()
	scope := common.NewScope()
	forms := make([]common.Syntax, len(result[common.Symbol("body")].([]interface{})))
	for i, syntax := range result[common.Symbol("body")].([]interface{}) {
		forms[i] = syntax.(common.Syntax).Push(scope, phase)
	}
	return expandBody(ctx, forms, mark, scope)
}

func transformBodyBegin(ctx BodyExpansionContext, syntax common.Syntax, mark *common.M, scope *common.Scope) (r6rs.CoreForm, error) {
	result, ok := patternBegin.Match(syntax)
	if !ok {
		return nil, fmt.Errorf("%v: bad syntax", syntaxKeywordForErrMsg(syntax))
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

func transformBodyDefineSyntax(ctx BodyExpansionContext, syntax common.Syntax, mark *common.M, scope *common.Scope) (r6rs.CoreForm, error) {
	result, ok := patternDefineSyntax.Match(syntax)
	if !ok {
		return nil, fmt.Errorf("%v: bad syntax", syntaxKeywordForErrMsg(syntax))
	}
	id, ok := result[common.Symbol("id")].(common.Syntax).Identifier()
	if !ok {
		return nil, fmt.Errorf("%v: bad syntax", syntaxKeywordForErrMsg(syntax))
	}
	syntax = result[common.Symbol("transformer")].(common.Syntax).Next()
	coreForm, err := ctx.Expander.(baseExpander).globalCtx().Expand(syntax)
	if err != nil {
		return nil, err
	}
	expression, err := coreForm.CpsTransform(r6rs.NewCpsTransformContext([]r6rs.Global{}))
	if err != nil {
		return nil, err
	}
	datum, err := common.Evaluate(common.NewEvaluationContext(), expression)
	if err != nil {
		return nil, err
	}
	transformer, ok := datum.(common.Procedure)
	if !ok {
		return nil, fmt.Errorf("%v: transformer is not a procedure", syntaxKeywordForErrMsg(syntax))
	}
	phase := result[common.Symbol("define-syntax")].(common.Syntax).IdentifierOrDie().Phase()
	_, binding := common.Bind(id, scope, phase)
	(&ctx.Env).Extend(binding, common.NewSyntacticAbstraction(transformer))
	return expandBody(ctx.ExpansionContext, ctx.Rest, mark, scope)
}
