package runtime

import (
	"fmt"
	"os"
	"path/filepath"
	"runtime"

	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/read"
)

var PatternTopLevelProgramImportForm = common.MustCompileSimplePattern(read.MustReadDatum("(import import-spec ...)"), common.Symbol("import"))

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
	r.provisions[ns] = &provision{library, common.EmptyBindingsFrame, false}
	return nil
}

func (r *Runtime) MustProvide(library *Library) {
	err := r.Provide(library)
	if err != nil {
		panic(err)
	}
}

func (r *Runtime) Bind(name []common.Symbol, bindings common.BindingsFrame) error {
	ns := nameString(name)
	prov, ok := r.provisions[ns]
	if !ok {
		return fmt.Errorf("runtime: cannot bind unknown library %v", ns)
	}
	prov.exports = bindings
	return nil
}

func (r *Runtime) MustBind(name []common.Symbol, bindings common.BindingsFrame) {
	err := r.Bind(name, bindings)
	if err != nil {
		panic(err)
	}
}

func (r *Runtime) Execute(topLevelProgram []common.Syntax, nullSourceLocationTree common.SourceLocationTree) error {
	result, ok := PatternTopLevelProgramImportForm.Match(topLevelProgram[0])
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

func (r *Runtime) ExecuteFile(name string) error {
	_, filename, _, ok := runtime.Caller(1)
	if !ok {
		return fmt.Errorf("failed to determine source location")
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

func (r *Runtime) BindingsFor(name []common.Symbol) (common.BindingsFrame, error) {
	prov, err := r.provisionFor(name)
	if err != nil {
		return common.EmptyBindingsFrame, err
	}
	err = r.instantiate(prov)
	if err != nil {
		return common.EmptyBindingsFrame, err
	}
	return prov.exports, nil
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
	if !prov.exports.Empty() {
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
	syntaxes := append(prov.library.body, common.NewSyntax(common.NewWrappedSyntax(common.Void, &prov.library.nullSourceLocationTree)))
	scopeSet := common.NewScopeSet()
	frame := common.NewFrame()
	stack := common.NewStack(frame)
	for i := range subProvs {
		err := r.instantiate(subProvs[i])
		if err != nil {
			return err
		}
		err = subProvs[i].exports.Load(resolutions[i].levels, scopeSet, frame, resolutions[i].identifierSpec)
		if err != nil {
			return instantiationError{prov.library.name, err}
		}
	}
	body := scopeSet.Apply(common.Body(prov.library.nullSourceLocationTree, syntaxes...))
	frameTemplate := frame.Template()
	expression, err := r.compiler.Compile(body, scopeSet[0], &frameTemplate, stack)
	if err != nil {
		return instantiationError{prov.library.name, err}
	}
	frame.Grow(frameTemplate)
	_, err = common.Evaluate(stack, expression)
	if err != nil {
		return instantiationError{prov.library.name, err}
	}
	prov.exports = common.NewBindingsFrame()
	for _, exportSpec := range prov.library.exportSpecs {
		scopeSet.Save(prov.exports, frame, exportSpec.internal, exportSpec.external)
	}
	return nil
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

func versionString(version []subVersion) string {
	var data []common.Datum
	for _, subV := range version {
		data = append(data, common.Number(fmt.Sprintf("%d", subV)))
	}
	return common.Write(list(data...))
}

func list(data ...common.Datum) common.Datum {
	if len(data) == 0 {
		return common.Null
	} else {
		return common.Pair{data[0], list(data[1:]...)}
	}
}

type provision struct {
	library *Library
	exports common.BindingsFrame
	visited bool
}

type identifierBinding struct {
	internal common.Symbol
	external common.Symbol
}