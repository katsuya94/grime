package r6rs_base

import (
	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/lib/r6rs"
)

func expandBody(ctx r6rs.ExpansionContext, forms []common.Syntax, mark *common.M) (r6rs.CoreForm, error) {
	output := common.Pair{r6rs.SequenceId.Mark(mark).WrappedSyntax, list(syntaxDatumSlice(forms)...)}
	return ctx.Expand(common.NewSyntax(output))
}
