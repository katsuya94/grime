package lib

import (
	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/lib/base"
	"github.com/katsuya94/grime/lib/core"
	"github.com/katsuya94/grime/lib/derived"
	"github.com/katsuya94/grime/lib/grime"
	"github.com/katsuya94/grime/runtime"
)

type libraryBinding struct {
	library  *runtime.Library
	bindings common.BindingsFrame
}

var libraryBindings = []libraryBinding{
	{core.Library, core.Bindings},
	{derived.Library, common.EmptyBindingsFrame},
	{base.Library, common.EmptyBindingsFrame},
	{grime.Library, common.EmptyBindingsFrame},
}

var StandardLibraryName = []common.Symbol{common.Symbol("grime")}

var Runtime *runtime.Runtime

func init() {
	Runtime = runtime.NewRuntime(core.NewCompiler())
	for _, libraryBinding := range libraryBindings {
		err := Runtime.Provide(libraryBinding.library)
		if err != nil {
			panic(err)
		}
		if !libraryBinding.bindings.Empty() {
			err := Runtime.Bind(libraryBinding.library.Name(), libraryBinding.bindings)
			if err != nil {
				panic(err)
			}
		}
	}
}
