package r6rs_base

import (
	"fmt"

	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/lib/r6rs"
	"github.com/katsuya94/grime/read"
)

var syntaxTransformer = r6rs.NewCoreTransformer(transformSyntax)
var patternSyntax = common.MustCompileSimplePattern(read.MustReadDatum("(syntax template)"))

func transformSyntax(ctx common.ExpansionContext, syntax common.Syntax, mark *common.M) (common.CoreForm, error) {
	result, ok := patternSyntax.Match(syntax)
	if !ok {
		return nil, fmt.Errorf("syntax: bad syntax")
	}
	syntaxTemplate, err := common.CompileSyntaxTemplate(result[common.Symbol("template")].(common.Syntax), ctx.Env)
	if err != nil {
		return nil, err
	}
	return syntaxForm{syntaxTemplate, mark}, nil
}

type syntaxForm struct {
	template common.SyntaxTemplate
	mark     *common.M
}

func (f syntaxForm) CpsTransform(ctx *common.CpsTransformContext) (common.Expression, error) {
	indices := make([]int, len(f.template.Bindings))
	for i, binding := range f.template.Bindings {
		index, err := ctx.Index(binding.Identifier())
		if err != nil {
			return nil, err
		}
		indices[i] = index
	}
	return syntax{f.template, indices}, nil
}

type syntax struct {
	template common.SyntaxTemplate
	indices  []int
}

func (e syntax) Evaluate(ctx common.EvaluationContext, c common.Continuation) (common.Evaluation, error) {
	matches := make(map[*common.Binding]interface{}, len(e.indices))
	for i := range e.indices {
		matches[e.template.Bindings[i]] = ctx.Get(e.indices[i]).Get()
	}
	datum, err := e.template.Evaluate(matches)
	if err != nil {
		return common.ErrorC(err)
	}
	return common.CallC(c, datum)
}
