package r6rs_base

import (
	"fmt"

	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/lib/r6rs"
	"github.com/katsuya94/grime/read"
)

var syntaxTransformer = r6rs.NewCoreTransformer(transformSyntax)
var patternSyntax = common.MustCompileSimplePattern(read.MustReadDatum("(syntax template)"))

func transformSyntax(ctx r6rs.ExpansionContext, syntax common.Syntax, mark *common.M) (r6rs.CoreForm, error) {
	result, ok := patternSyntax.Match(syntax)
	if !ok {
		return nil, fmt.Errorf("syntax: bad syntax")
	}
	template := result[common.Symbol("template")].(common.Syntax)
	return syntaxForm{template, mark}, nil
}

type syntaxForm struct {
	template common.Syntax
	mark     *common.M
}

func (f syntaxForm) CpsTransform(ctx r6rs.CpsTransformContext) (common.Expression, error) {
	return syntax{f.template}, nil
}

type syntax struct {
	template common.Syntax
}

func (e syntax) Evaluate(ctx common.EvaluationContext, c common.Continuation) (common.Evaluation, error) {
	return common.CallC(c, e.template.Datum())
}
