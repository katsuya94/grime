package runtime

import (
	"fmt"

	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/lib/r6rs_base"
	"github.com/katsuya94/grime/read"
)

type Runtime struct {
	libraries []*Library
}

func NewRuntime() *Runtime {
	return &Runtime{nil}
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
	return (&run{r, nil}).runWithImports(body, importSpecs)
}

func (r *Runtime) library(libraryName []common.Symbol) *Library {
	for _, library := range r.libraries {
		if nameEqual(library.name, libraryName) {
			return library
		}
	}
	return nil
}

type libraryScope struct {
	name     []common.Symbol
	bindings map[common.Symbol]*common.Binding
}

type run struct {
	runtime       *Runtime
	libraryScopes []libraryScope
}

func (r *run) runWithImports(body []common.Syntax, importSpecs []importSpec) error {
	for _, importSpec := range importSpecs {
		bindings, err := r.instantiate(importSpec.libraryName())
		if err != nil {
			return err
		}
	}
	return nil
}

func (r *run) instantiate(libraryName []common.Symbol) (map[common.Symbol]*common.Binding, error) {
	for _, libraryScope := range r.libraryScopes {
		if nameEqual(libraryScope.name, libraryName) {
			if libraryScope.bindings == nil {
				return nil, fmt.Errorf("import cycle")
			}
			return libraryScope.bindings, nil
		}
	}
	r.libraryBindings = append(r.libraryBindings, libraryScope{libraryName, nil})
	libvray
	library := r.runtime.library(libraryName)
	if library == nil {
		return nil, fmt.Errorf("unknown library: %s", nameString)
	}
	scope := common.NewScope()
	body := append(library.body, common.NewSyntax(common.NewWrappedSyntax(common.Void, library.nullSourceLocationTree)))
	coreForm, err := r6rs_base.ExpandBody(body, common.Environment{}, scope)
	if err != nil {
		return nil, err
	}
	expression, err := coreForm.CpsTransform(common.NewCpsTransformContext(nil))
	if err != nil {
		return nil, err
	}
	_, err = common.Evaluate(common.NewEvaluationContext(), expression)
	if err != nil {
		return nil, err
	}

	for _, idBinding := range library.exportSpecs {
		id := common.NewIdentifier(idBinding.internal)
		id = id.Push(scope).IdentifierOrDie()
		binding := id.Binding()
		if binding == nil {
			return nil, fmt.Errorf("cannot export unbound identifier: %s", idBinding.internal)
		}

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

func nameEqual(a, b []common.Symbol) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}

func list(data ...common.Datum) common.Datum {
	if len(data) == 0 {
		return common.Null
	} else {
		return common.Pair{data[0], list(data[1:]...)}
	}
}
