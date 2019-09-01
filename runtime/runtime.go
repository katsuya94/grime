package runtime

import (
	"fmt"

	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/read"
)

type Runtime struct {
	libraries map[string]*Library
}

func NewRuntime() *Runtime {
	return &Runtime{[]*Library{}}
}

var patternTopLevelProgramImportForm = common.MustCompileSimplePattern(read.MustReadDatum("(import import-spec ...)"), common.Symbol("import"))

func (r *Runtime) Run(topLevelProgram []common.Syntax, nullSourceLocationTree common.SourceLocationTree) error {
	result, ok := patternTopLevelProgramImportForm.Match(topLevelProgram[0])
	if !ok {
		return fmt.Errorf("runtime: malformed top-level program import form")
	}
	var importSpecs []importSpec
	for _, d := range result[common.Symbol("import-spec")].([]interface{}) {
		importSpec, err := newImportSpec(d.(common.Syntax))
		if err != nil {
			return nil
		}
		importSpecs = append(importSpecs, importSpec)
	}
	body := topLevelProgram[1:]
	return &run{r, nil}.runWithImports(body, importSpecs)
}

func (r *Runtime) findLibrary(libraryName []common.Symbol) *Library {
	return r.libraries[nameString(libraryName)]
}

type run struct {
	runtime       *Runtime
	libraryScopes map[string]*common.Scope
}

func (r *run) runWithImports(body []common.Syntax, importSpecs []importSpec) error {
	for _, importSpec := range importSpecs {
		scope := r.libraryScope(importSpec.libraryName)
	}
}

func (r *run) libraryScope(name []common.Symbol) *Scope {
	scope := r.libraryScopes[nameString(importSpec.libraryName())]
	if scope == nil {
	}
}

func nameString(name []common.Symbol) string {
	return common.Write(list(nameData(name)...))
}

func nameData(name []common.Symbol) []common.Datum {
	var data []common.Datum
	for _, symbol := range name {
		data = append(data, symbol)
	}
	return data
}

func list(data ...common.Datum) common.Datum {
	if len(data) == 0 {
		return common.Null
	} else {
		return common.Pair{data[0], list(data[1:]...)}
	}
}
