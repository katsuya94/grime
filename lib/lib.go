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
	bindings common.BindingSet
}

var libraryBindings = []libraryBinding{
	{core.Library, core.Bindings},
	{derived.Library, nil},
	{base.Library, nil},
	{grime.Library, nil},
}

var StandardLibraryName = []common.Symbol{common.Symbol("grime")}

var Runtime *runtime.Runtime

func init() {
	Runtime = runtime.NewRuntime(core.Compile)
	for _, libraryBinding := range libraryBindings {
		err := Runtime.Provide(libraryBinding.library)
		if err != nil {
			panic(err)
		}
		if libraryBinding.bindings != nil {
			err := Runtime.Bind(libraryBinding.library.Name(), libraryBinding.bindings)
			if err != nil {
				panic(err)
			}
		}
	}
}
