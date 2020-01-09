package lib

import (
	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/lib/r6rs"
	"github.com/katsuya94/grime/lib/r6rs_base"
	"github.com/katsuya94/grime/runtime"
)

var StandardLibraryName = []common.Symbol{common.Symbol("grime")}

var Runtime = runtime.NewRuntime(r6rs_base.ExpanderFactory{})

func init() {
	Runtime.Add(r6rs.Library)
	Runtime.Add(r6rs_base.Library)
}
