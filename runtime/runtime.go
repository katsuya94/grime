package runtime

import (
	"fmt"

	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/read"
)

type Runtime struct {
	libraries []*Library
}

func NewRuntime() *Runtime {
	return &Runtime{[]*Library{}}
}

var patternTopLevelProgramImportForm = common.MustCompileSimplePattern(read.MustReadDatum("(import import-spec ...)"), common.Symbol("import"))

func (r *Runtime) Runtime(topLevelProgram []common.Syntax, nullSourceLocationTree common.SourceLocationTree) error {
	result, ok := patternTopLevelProgramImportForm.Match(topLevelProgram[0])
	if !ok {
		return fmt.Errorf("runtime: malformed top-level program import form")
	}
	var library Library
	for _, d := range result[common.Symbol("import-spec")].([]interface{}) {
		importSpec, err := newImportSpec(d.(common.Syntax))
		if err != nil {
			return nil
		}
		library.importSpecs = append(library.importSpecs, importSpec)
	}
	library.body = topLevelProgram[1:]
	library.nullSourceLocationTree = nullSourceLocationTree
	err := r.Provide(&library)
	if err != nil {
		return err
	}
	return r.instantiate(r.provisions["()"])
}
