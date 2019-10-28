package runtime

import (
	"fmt"

	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/read"
)

type Runtime struct {
	expanderFactory common.ExpanderFactory
	libraries       []Library
}

func NewRuntime(expanderFactory common.ExpanderFactory) *Runtime {
	return &Runtime{expanderFactory, nil}
}

var patternTopLevelProgramImportForm = common.MustCompileSimplePattern(read.MustReadDatum("(import import-spec ...)"), common.Symbol("import"))

func (r *Runtime) Add(library Library) {
	r.libraries = append(r.libraries, library)
}

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
	_, err := (&run{r, nil}).runWithImportSpecs(body, &nullSourceLocationTree, importSpecs, scope)
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

type portableLevel struct {
	portable common.Portable
	level    int
}

type libraryPortableLevels struct {
	name           []common.Symbol
	portableLevels map[common.Symbol][]portableLevel
}

type run struct {
	runtime                *Runtime
	libraryPortableLevelss []libraryPortableLevels
}

type portablePhase struct {
	portable common.Portable
	phase    int
}

func (r *run) runWithImportSpecs(body []common.Syntax, nullSourceLocationTree *common.SourceLocationTree, importSpecs []importSpec, scope *common.Scope) (map[*common.Binding][]portablePhase, error) {
	importablePhases, err := r.resolveImportSpecs(importSpecs)
	if err != nil {
		return nil, err
	}
	return r.runWithImportables(body, nullSourceLocationTree, importablePhases, scope)
}

func (r *run) resolveImportSpecs(importSpecs []importSpec) (map[common.Symbol][]portablePhase, error) {
	importablePhases := map[common.Symbol][]portablePhase{}
	for _, importSpec := range importSpecs {
		library, ok := r.runtime.library(importSpec.libraryName())
		if !ok {
			return nil, fmt.Errorf("unknown library: %s", nameString(importSpec.libraryName()))
		}
		importSpecResolution, ok := importSpec.resolve(library)
		if !ok {
			return nil, fmt.Errorf("version mismatch: %s", nameString(importSpec.libraryName()))
		}
		importableLevels, err := r.instantiate(library)
		if err != nil {
			return nil, err
		}
		transformer := importSpecResolution.identifierSpec.transformer()
		for external, importableLevels := range importableLevels {
			internal, ok := transformer.transform(external)
			if !ok {
				continue
			}
			for _, importableLevel := range importableLevels {
				for _, importLevel := range importSpecResolution.levels {
					importablePhases[internal] = append(importablePhases[internal], portablePhase{importableLevel.portable, importableLevel.level + importLevel})
				}
			}
		}
		if err := transformer.error(); err != nil {
			return nil, err
		}
	}
	return importablePhases, nil
}

type exportPhase struct {
	export common.Export
	phase  int
}

func (r *run) runWithImportables(body []common.Syntax, nullSourceLocationTree *common.SourceLocationTree, importablePhases map[common.Symbol][]portablePhase, scope *common.Scope) (map[*common.Binding][]portablePhase, error) {
	bindings := map[common.Symbol]*common.Binding{}
	cpsCtx := common.NewCpsTransformContext(nil)
	envProvider := common.MultiphaseEnvironmentProvider{}
	var imports []common.Import
	for name, importablePhases := range importablePhases {
		if _, ok := bindings[name]; !ok {
			_, binding := common.Bind(common.NewIdentifier(name), scope)
			bindings[name] = binding
		}
		binding := bindings[name]
		id := binding.Identifier()
		for _, importablePhase := range importablePhases {
			if _, ok := envProvider[importablePhase.phase]; !ok {
				envProvider[importablePhase.phase] = common.Environment{}
			}
			imprt := importablePhase.portable.Import(cpsCtx, envProvider[importablePhase.phase], id, binding)
			imports = append(imports, imprt)
		}
	}
	expander := r.runtime.expanderFactory.Expander(envProvider)
	expansionCtx := common.ExpansionContext{Expander: expander, Env: envProvider[0], Phase: 0}
	body = append(body, common.NewSyntax(common.NewWrappedSyntax(common.Void, nullSourceLocationTree)))
	coreForm, env, err := expander.ExpandBody(expansionCtx, body, scope)
	if err != nil {
		return nil, err
	}
	envProvider[0] = env
	ids := scope.Identifiers()
	exportPhases := map[*common.Binding][]exportPhase{}
	for _, id := range ids {
		binding := id.Binding()
		for phase, env := range envProvider {
			role := env[binding]
			if role == nil {
				continue
			}
			export, err := role.Export(cpsCtx, id)
			if err != nil {
				return nil, err
			}
			exportPhases[binding] = append(exportPhases[binding], exportPhase{export, phase})
		}
	}
	expression, err := coreForm.CpsTransform(cpsCtx)
	if err != nil {
		return nil, err
	}
	evalCtx := cpsCtx.EvaluationContextTemplate().New()
	for _, imprt := range imports {
		imprt.Provide(evalCtx)
	}
	_, err = common.Evaluate(evalCtx, expression)
	if err != nil {
		return nil, err
	}
	exportablePhases := map[*common.Binding][]portablePhase{}
	for binding, exportPhases := range exportPhases {
		exportablePhases[binding] = make([]portablePhase, len(exportPhases))
		for i := range exportPhases {
			exportablePhases[binding][i] = portablePhase{exportPhases[i].export.Portable(evalCtx), exportPhases[i].phase}
		}
	}
	return exportablePhases, nil
}

// TODO: accumulate a global environment that will allow identifiers introduced by syntax to resolve to roles

func (r *run) instantiate(library Library) (map[common.Symbol][]portableLevel, error) {
	for _, libraryPortableLevels := range r.libraryPortableLevelss {
		if nameEqual(libraryPortableLevels.name, library.Name()) {
			if libraryPortableLevels.portableLevels == nil {
				return nil, fmt.Errorf("import cycle")
			}
			return libraryPortableLevels.portableLevels, nil
		}
	}
	portableLevels := map[common.Symbol][]portableLevel{}
	r.libraryPortableLevelss = append(r.libraryPortableLevelss, libraryPortableLevels{library.Name(), portableLevels})
	importablePhases, err := r.resolveImportSpecs(library.importSpecs)
	if err != nil {
		return nil, err
	}
	for name, builtinImportablePhases := range library.builtin {
		importablePhases[name] = append(importablePhases[name], builtinImportablePhases...)
	}
	scope := common.NewScope()
	exportablePhases, err := r.runWithImportables(library.body, library.nullSourceLocationTree, importablePhases, scope)
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
		for _, exportablePhase := range exportablePhases[binding] {
			portableLevels[idBinding.external] = append(portableLevels[idBinding.external], portableLevel{exportablePhase.portable, exportablePhase.phase})
		}
	}
	return portableLevels, nil
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
