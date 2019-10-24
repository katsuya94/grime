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
	_, err := (&run{r, nil}).runWithImports(body, &nullSourceLocationTree, importSpecs, scope)
	return err
}

func (r *Runtime) library(libraryName []common.Symbol) (Library, bool) {
	for _, library := range r.libraries {
		if nameEqual(library.name, libraryName) {
			return library, true
		}
	}
	return Library{}, false
}

type envProviderImpl map[int]common.Environment

func (epi envProviderImpl) Environment(phase int) common.Environment {
	return epi[phase]
}

type libraryPortables struct {
	name      []common.Symbol
	portables map[common.Symbol]common.Portable
}

type run struct {
	runtime           *Runtime
	libraryPortabless []libraryPortables
}

func (r *run) runWithImports(body []common.Syntax, nullSourceLocationTree *common.SourceLocationTree, importSpecs []importSpec, scope *common.Scope) (map[*common.Binding]common.Portable, error) {
	bindings := map[common.Symbol]*common.Binding{}
	envProvider := envProviderImpl{}
	for _, importSpec := range importSpecs {
		library, ok := r.runtime.library(importSpec.libraryName())
		if !ok {
			return nil, fmt.Errorf("unknown library: %s", nameString(importSpec.libraryName()))
		}
		importSpecResolution, ok := importSpec.resolve(library)
		if !ok {
			return nil, fmt.Errorf("version mismatch: %s", nameString(importSpec.libraryName()))
		}
		importBindings, err := r.instantiate(library)
		if err != nil {
			return nil, err
		}
		transformer := importSpecResolution.identifierSpec.transformer()
		for external := range importBindings {
			internal, ok := transformer.transform(external)
			if ok {
				if _, ok := bindings[internal]; !ok {
					id := common.NewIdentifier(internal)
					_, binding := common.Bind(id, scope)
					bindings[internal] = binding
				}
				binding := bindings[internal]
				for _, level := range importSpecResolution.levels {
					// TODO: use exported levels of imports
					phase := level
					if _, ok := envProvider[phase]; !ok {
						envProvider[phase] = common.Environment{}
					}
					envProvider[phase][binding] = common.NewVariable()
				}
			}
		}
		if err := transformer.error(); err != nil {
			return nil, err
		}
	}
	body = append(body, common.NewSyntax(common.NewWrappedSyntax(common.Void, nullSourceLocationTree)))
	coreForm, env, err := r6rs_base.ExpandBody(body, envProvider, scope)
	if err != nil {
		return nil, err
	}
	cpsCtx := common.NewCpsTransformContext(nil)
	ids := scope.Identifiers()
	exports := make([]common.Export, len(ids))
	for i := range ids {
		role := env[ids[i].Binding()]
		exports[i], err = role.Export(cpsCtx, ids[i])
		if err != nil {
			return nil, err
		}
	}
	expression, err := coreForm.CpsTransform(cpsCtx)
	if err != nil {
		return nil, err
	}
	evalCtx := cpsCtx.EvaluationContextTemplate().New()
	_, err = common.Evaluate(evalCtx, expression)
	if err != nil {
		return nil, err
	}
	portables := map[*common.Binding]common.Portable{}
	for i := range ids {
		portables[ids[i].Binding()] = exports[i].Portable(evalCtx)
	}
	return portables, nil
}

func (r *run) instantiate(library Library) (map[common.Symbol]common.Portable, error) {
	for _, libraryPortables := range r.libraryPortabless {
		if nameEqual(libraryPortables.name, library.Name()) {
			if libraryPortables.portables == nil {
				return nil, fmt.Errorf("import cycle")
			}
			return libraryPortables.portables, nil
		}
	}
	portables := map[common.Symbol]common.Portable{}
	r.libraryPortabless = append(r.libraryPortabless, libraryPortables{library.Name(), portables})
	scope := common.NewScope()
	portablesByBinding, err := r.runWithImports(library.body, library.nullSourceLocationTree, library.importSpecs, scope)
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
		portables[idBinding.external] = portablesByBinding[binding]
	}
	return portables, nil
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
