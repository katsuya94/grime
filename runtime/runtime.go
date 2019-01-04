package runtime

import (
	"fmt"
	"os"
	"path/filepath"
	"runtime"

	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/read"
	"github.com/katsuya94/grime/util"
)

var PatternTopLevelProgramImportForm = common.Pattern(read.MustReadDatum("(import import-spec ...)"))

type Runtime struct {
	compiler   common.Compiler
	provisions map[string]*provision
}

func NewRuntime(compiler common.Compiler) *Runtime {
	return &Runtime{compiler, make(map[string]*provision)}
}

func (r *Runtime) Provide(library *Library) error {
	ns := nameString(library.name)
	if _, ok := r.provisions[ns]; ok {
		return fmt.Errorf("runtime: library %v already provided", ns)
	}
	r.provisions[ns] = &provision{library, nil, false}
	return nil
}

func (r *Runtime) MustProvide(library *Library) {
	err := r.Provide(library)
	if err != nil {
		panic(err)
	}
}

func (r *Runtime) Bind(name []common.Symbol, bindings common.BindingSet) error {
	ns := nameString(name)
	prov, ok := r.provisions[ns]
	if !ok {
		return fmt.Errorf("runtime: cannot bind unknown library %v", ns)
	}
	prov.bindings = bindings
	return nil
}

func (r *Runtime) MustBind(name []common.Symbol, bindings common.BindingSet) {
	err := r.Bind(name, bindings)
	if err != nil {
		panic(err)
	}
}

func (r *Runtime) Execute(topLevelProgram []common.WrappedSyntax, nullSourceLocationTree common.SourceLocationTree) error {
	result, ok, err := common.MatchSyntax(topLevelProgram[0], PatternTopLevelProgramImportForm, map[common.Symbol]common.Location{
		common.Symbol("import"): nil,
	})
	if err != nil {
		return err
	} else if !ok {
		return fmt.Errorf("runtime: malformed top-level program import form")
	}
	var library Library
	for _, d := range result[common.Symbol("import-spec")].([]interface{}) {
		importSpec, err := newImportSpec(d.(common.WrappedSyntax))
		if err != nil {
			return nil
		}
		library.importSpecs = append(library.importSpecs, importSpec)
	}
	library.body = topLevelProgram[1:]
	library.nullSourceLocationTree = nullSourceLocationTree
	err = r.Provide(&library)
	if err != nil {
		return err
	}
	return r.instantiate(r.provisions["()"])
}

func (r *Runtime) ExecuteFile(name string) error {
	_, filename, _, ok := runtime.Caller(1)
	if !ok {
		fmt.Errorf("failed to determine source location")
	}
	sourcePath := filepath.Join(filepath.Dir(filename), fmt.Sprintf("%v.scm", name))
	f, err := os.Open(sourcePath)
	if err != nil {
		return err
	}
	syntaxes, nullSourceLocationTree, err := read.Read(sourcePath, f)
	if err != nil {
		return err
	}
	return r.Execute(syntaxes, nullSourceLocationTree)
}

func (r *Runtime) BindingsFor(name []common.Symbol) (common.BindingSet, error) {
	prov, err := r.provisionFor(name)
	if err != nil {
		return nil, err
	}
	err = r.instantiate(prov)
	if err != nil {
		return nil, err
	}
	return prov.bindings, nil
}

func (r *Runtime) provisionFor(name []common.Symbol) (*provision, error) {
	prov, ok := r.provisions[nameString(name)]
	if !ok {
		return nil, fmt.Errorf("runtime: no such library %v", nameString(name))
	}
	return prov, nil
}

type instantiationError struct {
	libraryName []common.Symbol
	err         error
}

func (err instantiationError) Error() string {
	if len(err.libraryName) == 0 {
		return fmt.Sprintf("runtime: %v", err.err)
	}
	return fmt.Sprintf("runtime: while instantiating %v: %v", nameString(err.libraryName), err.err)
}

func (r *Runtime) Instantiate(name []common.Symbol) error {
	prov, err := r.provisionFor(name)
	if err != nil {
		return err
	}
	return r.instantiate(prov)
}

func (r *Runtime) MustInstantiate(name []common.Symbol) {
	err := r.Instantiate(name)
	if err != nil {
		panic(err)
	}
}

func (r *Runtime) instantiate(prov *provision) error {
	if prov.bindings != nil {
		return nil
	}
	if prov.visited {
		return instantiationError{prov.library.name, fmt.Errorf("import cycle detected")}
	}
	prov.visited = true
	var (
		subProvs    []*provision
		resolutions []importSpecResolution
	)
	for _, importSpec := range prov.library.importSpecs {
		subProv, err := r.provisionFor(importSpec.libraryName())
		if err != nil {
			return err
		}
		resolution, ok := importSpec.resolve(subProv.library)
		if !ok {
			return instantiationError{
				prov.library.name,
				fmt.Errorf("version mismatch for library %v at version %v", nameString(importSpec.libraryName()), versionString(subProv.library.version)),
			}
		}
		subProvs = append(subProvs, subProv)
		resolutions = append(resolutions, resolution)
	}
	syntaxes := append(prov.library.body, common.NewWrappedSyntax(common.Void, &prov.library.nullSourceLocationTree))
	scopes := make(map[int]*common.Scope)
	scopes[0] = common.NewScope(0)
	for i := range subProvs {
		err := r.instantiate(subProvs[i])
		if err != nil {
			return err
		}
		bindings, err := resolutions[i].identifierSpec.resolve(subProvs[i].bindings)
		if err != nil {
			return instantiationError{prov.library.name, err}
		}
		for exportLevel, locations := range bindings {
			for name, location := range locations {
				phases := make(map[int]struct{})
				for _, importLevel := range resolutions[i].levels {
					phases[importLevel+exportLevel] = struct{}{}
				}
				for phase := range phases {
					scope, ok := scopes[phase]
					if !ok {
						scope = common.NewScope(phase)
					}
					err := scope.Set(common.NewIdentifier(name), location)
					if err != nil {
						return instantiationError{prov.library.name, err}
					}
				}
			}
		}
	}
	body := common.Body(prov.library.nullSourceLocationTree, syntaxes...)
	for _, scope := range scopes {
		body = body.Push(scope)
	}
	expression, err := r.compiler(body, scopes[0])
	if err != nil {
		return instantiationError{prov.library.name, err}
	}
	prov.bindings = make(common.BindingSet)
	for _, exportSpec := range prov.library.exportSpecs {
		exported := false
		for phase, scope := range scopes {
			prov.bindings[phase] = make(map[common.Symbol]common.Location)
			for name, location := range scope.Bindings() {
				if exportSpec.internal != name {
					continue
				}
				_, ok := prov.bindings[phase]
				if !ok {
					prov.bindings[phase] = make(map[common.Symbol]common.Location)
				}
				prov.bindings[phase][exportSpec.external] = location
				exported = true
			}
		}
		if !exported {
			return instantiationError{prov.library.name, fmt.Errorf("can't export unbound identifier %v", exportSpec.internal)}
		}
	}
	_, err = common.EvaluateOnce(expression)
	if err != nil {
		return instantiationError{prov.library.name, err}
	}
	return nil
}

func nameString(name []common.Symbol) string {
	return common.Write(util.List(nameData(name)...))
}

func nameData(name []common.Symbol) []common.Datum {
	var data []common.Datum
	for _, symbol := range name {
		data = append(data, symbol)
	}
	return data
}

func versionString(version []subVersion) string {
	var data []common.Datum
	for _, subV := range version {
		data = append(data, common.Number(fmt.Sprintf("%d", subV)))
	}
	return common.Write(util.List(data...))
}

type provision struct {
	library  *Library
	bindings common.BindingSet
	visited  bool
}

type identifierBinding struct {
	internal common.Symbol
	external common.Symbol
}
