package runtime

import (
	"fmt"

	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/lib/r6rs_base"
	"github.com/katsuya94/grime/read"
)

type Runtime struct {
	libraries []Library
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
	scope := common.NewScope()
	return (&run{r, nil}).runWithImports(body, &nullSourceLocationTree, importSpecs, scope)
}

func (r *Runtime) library(libraryName []common.Symbol) (Library, bool) {
	for _, library := range r.libraries {
		if nameEqual(library.name, libraryName) {
			return library, true
		}
	}
	return Library{}, false
}

type libraryBindings struct {
	name     []common.Symbol
	bindings map[common.Symbol]*common.Binding
}

type run struct {
	runtime          *Runtime
	libraryBindingss []libraryBindings
}

func (r *run) runWithImports(body []common.Syntax, nullSourceLocationTree *common.SourceLocationTree, importSpecs []importSpec, scope *common.Scope) error {
	bindings := map[int]map[common.Symbol]*common.Binding{}
	for _, importSpec := range importSpecs {
		library, ok := r.runtime.library(importSpec.libraryName())
		if !ok {
			return fmt.Errorf("unknown library: %s", nameString(importSpec.libraryName()))
		}
		importSpecResolution, ok := importSpec.resolve(library)
		if !ok {
			return fmt.Errorf("version mismatch: %s", nameString(importSpec.libraryName()))
		}
		importBindings, err := r.instantiate(library)
		if err != nil {
			return err
		}
		transformer := importSpecResolution.identifierSpec.transformer()
		for external, binding := range importBindings {
			internal, ok := transformer.transform(external)
			if ok {
				for _, level := range importSpecResolution.levels {
					if _, ok := bindings[level]; !ok {
						bindings[level] = map[common.Symbol]*common.Binding{}
					}
					bindings[level][internal] = binding
				}
			}
		}
	}
	body = append(body, common.NewSyntax(common.NewWrappedSyntax(common.Void, nullSourceLocationTree)))
	coreForm, err := r6rs_base.ExpandBody(body, common.Environment{}, scope)
	if err != nil {
		return err
	}
	expression, err := coreForm.CpsTransform(common.NewCpsTransformContext(nil))
	if err != nil {
		return err
	}
	_, err = common.Evaluate(common.NewEvaluationContext(), expression)
	if err != nil {
		return err
	}
	return nil
}

func (r *run) instantiate(library Library) (map[common.Symbol]*common.Binding, error) {
	for _, libraryBindings := range r.libraryBindingss {
		if nameEqual(libraryBindings.name, library.Name()) {
			if libraryBindings.bindings == nil {
				return nil, fmt.Errorf("import cycle")
			}
			return libraryBindings.bindings, nil
		}
	}
	r.libraryBindingss = append(r.libraryBindingss, libraryBindings{library.Name(), nil})
	scope := common.NewScope()
	r.runWithImports(library.body, library.nullSourceLocationTree, library.importSpecs, scope)
	bindings := map[common.Symbol]*common.Binding{}
	for _, idBinding := range library.exportSpecs {
		id := common.NewIdentifier(idBinding.internal)
		id = id.Push(scope).IdentifierOrDie()
		binding := id.Binding()
		if binding == nil {
			return nil, fmt.Errorf("cannot export unbound identifier: %s", idBinding.internal)
		}
		bindings[idBinding.external] = binding
	}
	return bindings, nil
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
