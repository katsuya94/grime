package cmd

import (
	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/lib/base"
	"github.com/katsuya94/grime/lib/core"
	"github.com/katsuya94/grime/lib/derived"
	"github.com/katsuya94/grime/lib/fmt"
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
	{fmt.Library, fmt.Bindings},
}

func newRuntime() (*runtime.Runtime, error) {
	rt := runtime.NewRuntime(core.Compile)
	for _, libraryBinding := range libraryBindings {
		err := rt.Provide(libraryBinding.library)
		if err != nil {
			return nil, err
		}
		if libraryBinding.bindings != nil {
			err := rt.Bind(core.Library.Name(), core.Bindings)
			if err != nil {
				return nil, err
			}
		}
	}
	return rt, nil
}
