package runtime

import (
	"fmt"

	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/read"
	"github.com/katsuya94/grime/util"
)

var PatternTopLevelProgramImportForm = read.MustReadString("(import import-spec ...)")[0]

type Runtime struct {
	libraries []*Library
}

func NewRuntime() *Runtime {
	return &Runtime{}
}

func (r *Runtime) Bind(lib *Library) error {
	for _, l := range r.libraries {
		if sameName(lib.name, l.name) {
			return fmt.Errorf("runtime: library %v already bound", util.Write(util.List(nameData(l.name)...)))
		}
	}
	r.libraries = append(r.libraries, lib)
	return nil
}

func (r *Runtime) Execute(topLevelProgram []common.Datum) error {
	result, ok, err := util.Match(topLevelProgram[0], PatternTopLevelProgramImportForm, map[common.Symbol]common.Binding{
		common.Symbol("import"): nil,
	})
	if err != nil {
		return err
	} else if !ok {
		return fmt.Errorf("runtime: malformed top-level program import form")
	}
	var library Library
	for _, d := range result[common.Symbol("import-spec")].([]interface{}) {
		importSpec, err := newImportSpec(d)
		if err != nil {
			return nil
		}
		library.importSpecs = append(library.importSpecs, importSpec)
	}
	library.body = topLevelProgram[1:]
	return r.Instantiate(&library)
}

func (r *Runtime) Instantiate(l *Library) error {
	var (
		libraries   []*Library
		resolutions []importSpecResolution
	)
	for _, importSpec := range l.importSpecs {
		var (
			library *Library
			res     importSpecResolution
			ok      = false
		)
		for _, library = range r.libraries {
			res, ok = importSpec.resolve(library)
			if ok {
				break
			}
		}
		if !ok {
			return fmt.Errorf("runtime: could not resolve import spec %v", util.Write(util.List(nameData(importSpec.libraryName())...)))
		}
		libraries = append(libraries, library)
		resolutions = append(resolutions, res)
	}
	return nil
}

type identifierBinding struct {
	external common.Symbol
	internal common.Symbol
}
