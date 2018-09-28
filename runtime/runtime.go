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
	r.Provide(&library)
	return r.instantiate(r.provisions["()"])
}

func (r *Runtime) instantiate(prov *provision) error {
	if prov.bindings != nil {
		return nil
	}
	if prov.visited {
		return fmt.Errorf("runtime: import cycle detected when attempting to instantiate %v", nameString(prov.library.name))
	}
	prov.visited = true
	var (
		subProvs    []*provision
		resolutions []importSpecResolution
	)
	for _, importSpec := range prov.library.importSpecs {
		subProv, ok := r.provisions[nameString(importSpec.libraryName())]
		var resolution importSpecResolution
		if ok {
			resolution, ok = importSpec.resolve(subProv.library)
		}
		if !ok {
			return fmt.Errorf("runtime: could not resolve library %v", nameString(importSpec.libraryName()))
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
			env = env.Set(internal, binding)
		}
	}
	form, err := eval.ExpandBody(env, prov.library.body)
	if err != nil {
		return err
	}
	expression, err := eval.Compile(env, form)
	if err != nil {
		return err
	}
	_, err = eval.EvaluateExpressionOnce(expression)
	return err
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

type provision struct {
	library  *Library
	bindings map[common.Symbol]common.Binding
	visited  bool
}

type identifierBinding struct {
	external common.Symbol
	internal common.Symbol
}
