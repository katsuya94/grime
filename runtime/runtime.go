package runtime

import (
	"fmt"

	"github.com/katsuya94/grime/eval"

	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/read"
	"github.com/katsuya94/grime/util"
)

var PatternTopLevelProgramImportForm = read.MustReadString("(import import-spec ...)")[0]

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

func (r *Runtime) Bind(name []common.Symbol, bindings map[common.Symbol]common.Binding) error {
	ns := nameString(name)
	prov, ok := r.provisions[ns]
	if !ok {
		return fmt.Errorf("runtime: cannot bind unknown library %v", ns)
	}
	prov.bindings = bindings
	return nil
}

func (r *Runtime) Execute(topLevelProgram []common.Datum) error {
	result, ok, err := util.Match(topLevelProgram[0], PatternTopLevelProgramImportForm, map[common.Symbol]common.Location{
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
	r.Provide(&library)
	return r.instantiate(r.provisions["()"])
}

func (r *Runtime) BindingsFor(name []common.Symbol) (map[common.Symbol]common.Binding, error) {
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

func (r *Runtime) instantiate(prov *provision) error {
	if prov.bindings != nil {
		return nil
	}
	if prov.visited {
		return fmt.Errorf("runtime: import cycle detected while attempting to instantiate %v", nameString(prov.library.name))
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
			return fmt.Errorf("runtime: version mismatch for library %v at version %v", nameString(importSpec.libraryName()), versionString(subProv.library.version))
		}
		subProvs = append(subProvs, subProv)
		resolutions = append(resolutions, resolution)
	}
	env := common.NewEnvironment(make(map[common.Symbol]common.Binding))
	for i := range subProvs {
		err := r.instantiate(subProvs[i])
		if err != nil {
			return err
		}
		for external, binding := range subProvs[i].bindings {
			internal, ok := resolutions[i].identifierSpec.resolve(external)
			if !ok {
				continue
			}
			var levels []int
			for _, importLevel := range resolutions[i].levels {
				for _, exportLevel := range binding.Levels {
					exists := false
					for _, level := range levels {
						if level == importLevel+exportLevel {
							exists = true
							break
						}
					}
					if !exists {
						levels = append(levels, importLevel+exportLevel)
					}
				}
			}
			env, err = env.Define(internal, levels, binding.Location)
			if err != nil {
				return err
			}
		}
	}
	expression, definitions, err := eval.CompileBody(env, append(prov.library.body, common.Void))
	if err != nil {
		return err
	}
	prov.bindings = make(map[common.Symbol]common.Binding)
	for _, exportSpec := range prov.library.exportSpecs {
		binding, ok := definitions[exportSpec.internal]
		if !ok {
			return fmt.Errorf("runtime: can't export unbound identifier %v", exportSpec.internal)
		}
		prov.bindings[exportSpec.external] = binding
	}
	_, err = eval.EvaluateExpressionOnce(expression)
	if err != nil {
		return err
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
	bindings map[common.Symbol]common.Binding
	visited  bool
}

type identifierBinding struct {
	internal common.Symbol
	external common.Symbol
}
