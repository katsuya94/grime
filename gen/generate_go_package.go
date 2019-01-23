package gen

import (
	"bytes"
	"fmt"
	"go/token"
	"go/types"
	"sort"
	"strings"
	"text/template"

	"golang.org/x/tools/go/packages"
)

var goFileTemplate = template.Must(template.New("goFile").Parse(`
// This file was auto-generated by Grime.

package {{.Name}}

import (
{{range .StandardImports -}}
	{{"\t"}}{{if ne .Name ""}}{{.Name}} {{end}}"{{.PkgPath}}"
{{end -}}
{{range .OtherImports -}}
	{{"\t"}}{{if ne .Name ""}}{{.Name}} {{end}}"{{.PkgPath}}"
{{end -}}
)

var Library = runtime.MustNewEmptyLibrary([]common.Symbol{
{{range .PkgPathSegments -}}
	{{"\t"}}common.Symbol("{{.}}"),
{{end -}}
}, []int{})

var Bindings = common.BindingSet{
	0: map[common.Symbol]common.Location{
	{{range .Funcs -}}
		{{"\t"}}common.Symbol("{{.QualifiedName}}"): &common.Variable{common.Function({{.InternalName}})},
	{{end -}}
	{{range .Structs -}}
		{{"\t"}}common.Symbol("{{.QualifiedName}}"): &common.Variable{common.Function({{.InternalName}})},
	{{end -}}
	},
}

{{range .Funcs -}}
func {{.InternalName}}(c common.Continuation, args ...common.Datum) (common.EvaluationResult, error) {
	{{if len .Params -}}
	{{if .Type.Variadic -}}
	valid := len(args) >= {{len .Params}} - 1
	{{else -}}
	valid := len(args) == {{len .Params}}
	{{end -}}
	{{range $i, $param := .Params -}}
	{{if $param.Variadic -}}
	var {{$param.LocalName}} []{{$param.QualifiedType}}
	for _, arg := range args[{{$i}}:] {
		if !valid {
			break
		}
		var elem {{$param.QualifiedType}}
		{{if ne $param.CoercibleType "" -}}
		coerced := arg
		if coercible, ok := coerced.({{$param.CoercibleType}}); ok {
			coerced = {{$param.QualifiedType}}(coercible)
		}
		elem, valid = coerced.({{$param.QualifiedType}})
		{{else -}}
		elem, valid = arg.({{$param.QualifiedType}})
		{{end -}}
		{{$param.LocalName}} = append({{$param.LocalName}}, elem)
	}
	{{else -}}
	var {{$param.LocalName}} {{$param.QualifiedType}}
	if valid {
	{{if ne $param.CoercibleType "" -}}
		{{"\t"}}coerced := args[{{$i}}]
		if coercible, ok := coerced.({{$param.CoercibleType}}); ok {
			coerced = {{$param.QualifiedType}}(coercible)
		}
		{{$param.LocalName}}, valid = coerced.({{$param.QualifiedType}})
	{{else -}}
		{{$param.LocalName}}, valid = args[{{$i}}].({{$param.QualifiedType}})
	{{end -}}
	}
	{{end -}}
	{{end -}}
	if !valid {
		var argTypes []string
		for _, arg := range args {
			argTypes = append(argTypes, reflect.TypeOf(arg).String())
		}
		return common.ErrorC(fmt.Errorf("{{.QualifiedName}}: expected ({{.ParamTypes}}), got (%v)", strings.Join(argTypes, ", ")))
	}
	{{end -}}
	{{range $i, $_ := .Results}}{{if gt $i 0}}, {{end}}r{{$i}}{{end}}{{if gt (len .Results) 0}} := {{end}}{{.QualifiedName}}(
	{{range .Params -}}
		{{"\t"}}{{if .Variadic -}}
		{{.LocalName}}...,
		{{- else -}}
		{{.LocalName}},
		{{- end}}
	{{end -}}
	)
	return common.CallC(c, util.List({{range $i, $_ := .Results}}{{if gt $i 0}}, {{end}}r{{$i}}{{end}}))
}

{{end -}}

{{range .Interfaces}}
type {{.InternalName}} struct {
{{range .Methods -}}
	{{"\t"}}{{.Field.InternalName}} common.Procedure
{{end -}}
}

{{with $interface := . -}}
{{range .Methods -}}
func (i {{$interface.InternalName}}) {{.Name}}({{range $i, $param := .Params}}{{if gt $i 0}}, {{end}}{{$param.LocalName}} {{$param.QualifiedType}}{{end}}) ({{range $i, $result := .Results}}{{if gt $i 0}}, {{end}}{{$result.QualifiedType}}{{end}}) {
	results, err := util.Invoke(i.{{.Field.InternalName}}{{range $i, $param := .Params}}, {{$param.LocalName}}{{end}})
	{{if ge .TrailingErrorIndex 0 -}}
	{{range $i, $result := .Results -}}
	var r{{$i}} {{$result.QualifiedType}}
	{{end -}}
	if err != nil {
		r{{.TrailingErrorIndex}} = err
		return {{range $i, $_ := .Results}}{{if gt $i 0}}, {{end}}r{{$i}}{{end}}
	}
	s, err := util.Slice(results)
	if err != nil {
		r{{.TrailingErrorIndex}} = err
		return {{range $i, $_ := .Results}}{{if gt $i 0}}, {{end}}r{{$i}}{{end}}
	}
	{{else -}}
	if err != nil {
		panic(err)
	}
	s, err := util.Slice(results)
	if err != nil {
		panic(err)
	}
	{{end -}}
	valid := len(s) == {{len .Results}}
	{{with $method := . -}}
	{{range $i, $result := .Results -}}
	{{if not (ge $method.TrailingErrorIndex 0) -}}
	var r{{$i}} {{$result.QualifiedType}}
	{{end -}}
	if valid {
	{{if ne $result.CoercibleType "" -}}
		{{"\t"}}coerced := s[{{$i}}]
		if coercible, ok := coerced.({{$result.CoercibleType}}); ok {
			coerced = {{$result.QualifiedType}}(coercible)
		}
		r{{$i}}, valid = coerced.({{$result.QualifiedType}})
	{{else -}}
		{{"\t"}}r{{$i}}, valid = s[{{$i}}].({{$result.QualifiedType}})
	{{end -}}
	}
	{{end -}}
	{{end -}}
	if !valid {
		var rTypes []string
		for _, r := range s {
			rTypes = append(rTypes, reflect.TypeOf(r).String())
		}
		panic(fmt.Sprintf("{{.QualifiedName}}: expected ({{.ParamTypes}}), got (%v)", strings.Join(rTypes, ", ")))
	}
	return {{range $i, $_ := .Results}}{{if gt $i 0}}, {{end}}r{{$i}}{{end}}
}

{{end -}}
{{end -}}
{{end -}}

{{range .Structs -}}
func {{.InternalName}}(c common.Continuation, args ...common.Datum) (common.EvaluationResult, error) {
	s := {{.QualifiedType}}{}
	m := map[common.Symbol]struct{}{}
	i := 0
	for ; i+1 < len(args); i += 2 {
		key, ok := args[i].(common.Symbol)
		if !ok {
			return common.ErrorC(fmt.Errorf("{{.QualifiedName}}: expected key, got %s", reflect.TypeOf(args[i]).String()))
		}
		if []byte(string(key))[0] != byte(':') {
			return common.ErrorC(fmt.Errorf("{{.QualifiedName}}: expected key, got %s", key))
		}
		if _, ok := m[key]; ok {
			return common.ErrorC(fmt.Errorf("{{.QualifiedName}}: duplicate key %s", key))
		}
		switch key {
	    {{with $struct := . -}}
		{{range .Fields -}}
		{{"\t"}}case common.Symbol(":{{.Name}}"):
			value, ok := args[i+1].({{.QualifiedType}})
			if !ok {
				return common.ErrorC(fmt.Errorf("{{$struct.QualifiedName}}: expected {{.Type}}, got %s", reflect.TypeOf(args[i+1]).String()))
			}
			s.{{.InternalName}} = value
		{{end -}}
		{{end -}}
		default:
			return common.ErrorC(fmt.Errorf("{{.QualifiedName}}: unrecognized key %s", key))
		}
		m[key] = struct{}{}
	}
	if i < len(args) {
		return common.ErrorC(fmt.Errorf("{{.QualifiedName}}: expected key-value pairs, got %d arguments", len(args)))
	}
	return common.CallC(c, s)
}

{{end -}}
`))

type goPackage struct {
	*packages.Package
	imports    *goImportSet
	Funcs      []*goFunc
	Interfaces []*goInterface
	Structs    []*goStruct
}

func newGoPackage(pkg *packages.Package) *goPackage {
	imports := newGoImportSet()
	imports.add("fmt")
	imports.add("reflect")
	imports.add("strings")
	imports.add("github.com/katsuya94/grime/common")
	imports.add("github.com/katsuya94/grime/runtime")
	imports.add("github.com/katsuya94/grime/util")
	imports.add(pkg.PkgPath)
	funcs := []*goFunc{}
	interfaces := []*goInterface{}
	structs := []*goStruct{}
	scope := pkg.Types.Scope()
	for _, name := range scope.Names() {
		switch obj := scope.Lookup(name).(type) {
		case *types.Func:
			if !obj.Exported() {
				continue
			}
			if obj.Type().(*types.Signature).Recv() != nil {
				continue
			}
			funcs = append(funcs, newGoFunc(imports, obj))
		case *types.TypeName:
			if !obj.Exported() {
				continue
			}
			switch t := obj.Type().Underlying().(type) {
			case *types.Interface:
				i := newGoInterface(imports, obj, t)
				interfaces = append(interfaces, i)
				structs = append(structs, i.implementation)
			case *types.Struct:
				fields := []*goVar{}
				for i := 0; i < t.NumFields(); i++ {
					if !t.Field(i).Exported() {
						continue
					}
					fields = append(fields, newGoVar(imports, t.Field(i).Name(), t.Field(i), false))
				}
				structs = append(structs, newGoStruct(imports, obj, obj, fields))
			default:
				panic(fmt.Sprintf("unhandled type %#v for %s", t, obj.Name()))
			}
		case *types.Var:
			fmt.Println("Var not yet implemented")
		case *types.Const:
			fmt.Println("Const not yet implemented")
		default:
			panic(fmt.Sprintf("unhandled object %#v", obj))
		}
	}
	return &goPackage{pkg, imports, funcs, interfaces, structs}
}

func (pkg *goPackage) PkgPathSegments() []string {
	return strings.Split(pkg.PkgPath, "/")
}

func (pkg *goPackage) StandardImports() []*goImport {
	return pkg.imports.imports(true)
}

func (pkg *goPackage) OtherImports() []*goImport {
	return pkg.imports.imports(false)
}

type goImportSet struct {
	pkgPaths map[string]string
	names    map[string]string
}

func newGoImportSet() *goImportSet {
	return &goImportSet{map[string]string{}, map[string]string{}}
}

func (set *goImportSet) add(pkgPath string) {
	if pkgPath == "" {
		return
	}
	if _, ok := set.names[pkgPath]; ok {
		return
	}
	name := defaultPkgName(pkgPath)
	for {
		if _, ok := set.pkgPaths[name]; !ok {
			break
		}
		name = fmt.Sprintf("_%s", name)
	}
	set.pkgPaths[name] = pkgPath
	set.names[pkgPath] = name
}

func (set *goImportSet) get(pkgPath string) string {
	if pkgPath == "" {
		return ""
	}
	return set.names[pkgPath]
}

func (set *goImportSet) imports(standard bool) []*goImport {
	mapping := map[string]string{}
	for name, pkgPath := range set.pkgPaths {
		parts := strings.Split(pkgPath, "/")
		if (len(parts) == 1) == standard {
			if name == parts[len(parts)-1] {
				mapping[pkgPath] = ""
			} else {
				mapping[pkgPath] = name
			}
		}
	}
	pkgPaths := []string{}
	for pkgPath := range mapping {
		pkgPaths = append(pkgPaths, pkgPath)
	}
	sort.Strings(pkgPaths)
	imports := []*goImport{}
	for _, pkgPath := range pkgPaths {
		imports = append(imports, &goImport{
			Name:    mapping[pkgPath],
			PkgPath: pkgPath,
		})
	}
	return imports
}

type goImport struct {
	Name    string
	PkgPath string
}

type goFunc struct {
	*types.Func
	Params  []*goVar
	Results []*goVar
}

func newGoFunc(imports *goImportSet, f *types.Func) *goFunc {
	signature := f.Type().(*types.Signature)
	params := make([]*goVar, signature.Params().Len())
	for i := 0; i < signature.Params().Len(); i++ {
		variadic := signature.Variadic() && i == signature.Params().Len()-1
		params[i] = newGoVar(imports, signature.Params().At(i).Name(), signature.Params().At(i), variadic)
	}
	results := make([]*goVar, signature.Results().Len())
	for i := 0; i < signature.Results().Len(); i++ {
		results[i] = newGoVar(imports, signature.Results().At(i).Name(), signature.Results().At(i), false)
	}
	return &goFunc{f, params, results}
}

func (f *goFunc) InternalName() string {
	return fmt.Sprintf("func_%s", f.Name())
}

func (f *goFunc) ParamTypes() string {
	var paramTypes []string
	for _, param := range f.Params {
		if param.Variadic {
			paramTypes = append(paramTypes, fmt.Sprintf("...%v", param.Type().(*types.Slice).Elem()))
		} else {
			paramTypes = append(paramTypes, param.Type().String())
		}
	}
	return strings.Join(paramTypes, ", ")
}

func (f *goFunc) QualifiedName() string {
	return qualifiedName(f.Pkg().Name(), f.Name())
}

func defaultPkgName(pkgPath string) string {
	parts := strings.Split(pkgPath, "/")
	return parts[len(parts)-1]
}

type goInterface struct {
	*types.Interface
	implementation *goStruct
	Methods        []*goMethod
}

var procedureType = types.NewNamed(types.NewTypeName(token.NoPos, types.NewPackage("github.com/katsuya94/grime/common", "common"), "Procedure", nil), nil, nil)

func newGoInterface(imports *goImportSet, name *types.TypeName, ifc *types.Interface) *goInterface {
	funcs := []*types.Func{}
	fields := []*goVar{}
	for i := 0; i < ifc.NumMethods(); i++ {
		f := ifc.Method(i)
		if !f.Exported() {
			continue
		}
		funcs = append(funcs, f)
		fields = append(fields, newGoVar(imports, f.Name(), types.NewField(token.NoPos, nil, fmt.Sprintf("field_%s", f.Name()), procedureType, false), false))
	}
	structName := types.NewTypeName(token.NoPos, types.NewPackage("", ""), fmt.Sprintf("interface_%s", name.Name()), nil)
	srt := newGoStruct(imports, name, structName, fields)
	methods := make([]*goMethod, len(fields))
	for i := range srt.Fields {
		methods[i] = newGoMethod(imports, funcs[i], srt.Fields[i])
	}
	return &goInterface{ifc, srt, methods}
}

func (ifc *goInterface) InternalName() string {
	return fmt.Sprintf("interface_%s", ifc.implementation.Name())
}

type goMethod struct {
	*goFunc
	Field *goVar
}

func newGoMethod(imports *goImportSet, f *types.Func, field *goVar) *goMethod {
	return &goMethod{newGoFunc(imports, f), field}
}

func (method *goMethod) TrailingErrorIndex() int {
	if len(method.Results) == 0 {
		return -1
	}
	t, ok := method.Results[len(method.Results)-1].Type().(*types.Named)
	if !ok {
		return -1
	}
	if t.Obj().Name() != "error" {
		return -1
	}
	return len(method.Results) - 1
}

type goStruct struct {
	QualifiedName string
	QualifiedType string
	Fields        []*goVar
}

func newGoStruct(imports *goImportSet, displayName *types.TypeName, typeName *types.TypeName, fields []*goVar) *goStruct {
	imports.add(typeName.Pkg().Path())
	return &goStruct{qualifiedName(displayName.Pkg().Name(), displayName.Name()), qualifiedName(imports.get(typeName.Pkg().Path()), typeName.Name()), fields}
}

func (srt *goStruct) Name() string {
	_, name := splitName(srt.QualifiedName)
	return name
}

func (srt *goStruct) InternalName() string {
	return fmt.Sprintf("struct_%s", srt.Name())
}

type goVar struct {
	*types.Var
	Name          string
	QualifiedType string
	CoercibleType string
	Variadic      bool
}

func newGoVar(imports *goImportSet, name string, v *types.Var, variadic bool) *goVar {
	t := v.Type()
	if variadic {
		t = t.(*types.Slice).Elem()
	}
	qualifiedType := computeQualifiedType(imports, t)
	coercibleType := ""
	switch qualifiedType {
	case "string":
		coercibleType = "common.String"
	}
	return &goVar{v, name, qualifiedType, coercibleType, variadic}
}

func computeQualifiedType(imports *goImportSet, t types.Type) string {
	switch t := t.(type) {
	case *types.Pointer:
		return fmt.Sprintf("*%s", computeQualifiedType(imports, t.Elem()))
	case *types.Named:
		pkgPath := ""
		if t.Obj().Pkg() != nil {
			pkgPath = t.Obj().Pkg().Path()
		}
		imports.add(pkgPath)
		return qualifiedName(imports.get(pkgPath), t.Obj().Name())
	case *types.Basic:
		return t.String()
	case *types.Interface:
		return t.String()
	case *types.Slice:
		return fmt.Sprintf("[]%s", computeQualifiedType(imports, t.Elem()))
	case *types.Signature:
		params := make([]string, t.Params().Len())
		for i := 0; i < t.Params().Len(); i++ {
			params[i] = computeQualifiedType(imports, t.Params().At(i).Type())
		}
		results := make([]string, t.Results().Len())
		for i := 0; i < t.Results().Len(); i++ {
			results[i] = computeQualifiedType(imports, t.Results().At(i).Type())
		}
		return fmt.Sprintf("func(%s) (%s)", strings.Join(params, ", "), strings.Join(results, ", "))
	default:
		panic(fmt.Sprintf("unhandled type %#v", t))
	}
}

func (v *goVar) InternalName() string {
	return v.Var.Name()
}

func (v *goVar) LocalName() string {
	return fmt.Sprintf("var_%s", v.InternalName())
}

func splitName(name string) (string, string) {
	a := strings.Split(name, ".")
	if len(a) == 1 {
		return "", a[0]
	}
	return a[0], a[1]
}

func qualifiedName(pkgName string, name string) string {
	if pkgName == "" {
		return name
	}
	return fmt.Sprintf("%v.%v", pkgName, name)
}

func generateGoPackage(pattern string) (bool, error) {
	pkg, err := loadPackage(pattern)
	if err != nil {
		return false, err
	} else if pkg == nil {
		return false, nil
	}
	buf := &bytes.Buffer{}
	err = goFileTemplate.Execute(buf, newGoPackage(pkg))
	if err != nil {
		return false, err
	}
	err = parseCheckWrite(fmt.Sprintf("%v.go", pkg.Name), generatedPackagePath(pkg.PkgPath), buf.String())
	if err != nil {
		return false, err
	}
	return true, nil
}
