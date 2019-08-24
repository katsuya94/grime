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

func expandBody(ctx r6rs.ExpansionContext, forms []common.Syntax, mark *common.M) (r6rs.CoreForm, error) {
	output := common.Pair{r6rs.SequenceId.Mark(mark).WrappedSyntax, list(syntaxDatumSlice(forms)...)}
	return ctx.Expand(common.NewSyntax(output))
}
