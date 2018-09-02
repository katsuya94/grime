package runtime

import (
	"fmt"

	"github.com/katsuya94/grime/util"
)

type Runtime struct {
	libraryBindings []libraryBinding
}

func NewRuntime() *Runtime {
	return &Runtime{}
}

func (r *Runtime) Bind(l *Library) error {
	for _, lBinding := range r.libraryBindings {
		if sameName(l, lBinding.library) {
			return fmt.Errorf("runtime: library %v already bound", util.List(l.name))
		}
	}
	r.libraryBindings = append(r.libraryBindings)
	return nil
}

func sameName(l1 *Library, l2 *Library) bool {
	if len(l1.name) != len(l2.name) {
		return false
	}
	for i := range l1.name {
		if l1.name[i] != l2.name[i] {
			return false
		}
	}
	return true
}

type libraryBinding struct {
	library  *Library
	instance *LibraryInstance
}
