package r6rs_base

import (
	"fmt"

	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/lib/r6rs"
)

type bodyTransformer struct {
	name string
}

func (t bodyTransformer) Call(c common.Continuation, args ...common.Datum) (common.Evaluation, error) {
	return common.ErrorC(fmt.Errorf("%v in expression context", t.name))
}

var (
	transformDefineSyntax = &bodyTransformer{"define-syntax"}
)

func expandBody(ctx r6rs.ExpansionContext, forms []common.Syntax, mark *common.M) (r6rs.CoreForm, error) {
	output := common.Pair{r6rs.SequenceId.Mark(mark).WrappedSyntax, list(syntaxDatumSlice(forms)...)}
	return ctx.Expand(common.NewSyntax(output))
}
