package runtime

import (
	"fmt"
	"os"
	"path/filepath"
	"runtime"

	"github.com/katsuya94/grime/eval"

	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/read"
	"github.com/katsuya94/grime/util"
)

var PatternTopLevelProgramImportForm = common.Pattern(read.MustReadString("(import import-spec ...)")[0])

type Runtime struct {
	provisions map[string]*provision
}

func NewRuntime() *Runtime {
	return &Runtime{make(map[string]*provision)}
}

func (r *Runtime) Provide(library *Library) error {
	ns := nameString(library.name)
	if _, ok := r.provisions[ns]; ok {
		return fmt.Errorf("runtime: library %v already provided", ns)
	}
	r.provisions[ns] = &provision{library, nil, false}
	return nil
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

func (r *Runtime) Execute(topLevelProgram []common.WrappedSyntax) error {
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
	r.Provide(&library)
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
	data, err := read.Read(f)
	if err != nil {
		return err
	}
	var topLevelProgram []common.WrappedSyntax
	for _, d := range data {
		topLevelProgram = append(topLevelProgram, common.NewWrappedSyntax(d))
	}
	return r.Execute(topLevelProgram)
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
	env := common.EmptyEnvironment
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
			for id, location := range locations {
				levelSet := make(map[int]bool)
				for _, importLevel := range resolutions[i].levels {
					levelSet[importLevel+exportLevel] = true
				}
				var levels []int
				for level := range levelSet {
					levels = append(levels, level)
				}
				env, err = env.Define(id, levels, location)
				if err != nil {
					return instantiationError{prov.library.name, err}
				}
			}
		}
	}
	body := append(prov.library.body, common.NewWrappedSyntax(common.Void))
	var forms []common.Datum
	for _, syntax := range body {
		forms = append(forms, syntax)
	}
	expression, definitions, err := eval.CompileBody(env, forms)
	if err != nil {
		return instantiationError{prov.library.name, err}
	}
	prov.bindings = make(common.BindingSet)
	for _, exportSpec := range prov.library.exportSpecs {
		exported := false
		for exportLevel, locations := range definitions {
			location, ok := locations[exportSpec.internal]
			if !ok {
				continue
			}
			exported = true
			if _, ok := prov.bindings[exportLevel]; !ok {
				prov.bindings[exportLevel] = make(map[common.Symbol]common.Location)
			}
			prov.bindings[exportLevel][exportSpec.external] = location
		}
		if !exported {
			return instantiationError{prov.library.name, fmt.Errorf("can't export unbound identifier %v", exportSpec.internal)}
		}
	}
	_, err = eval.EvaluateExpressionOnce(expression)
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
