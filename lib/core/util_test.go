package core_test

import (
	"github.com/katsuya94/grime/common"
	. "github.com/katsuya94/grime/lib/core"
)

func expandNever(Compiler, common.Syntax) (common.Syntax, bool, error) {
	return common.Syntax{}, false, nil
}

func expressionCompileIdentity(_ Compiler, form common.Syntax) (common.Expression, error) {
	return form.Datum().(common.Expression), nil
}
