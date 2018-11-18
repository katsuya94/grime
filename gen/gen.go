package main

import (
	"bytes"
	"fmt"
	"go/ast"
	"go/format"
	"go/importer"
	"go/parser"
	"go/token"
	"go/types"
	"os"
	"os/exec"
	"path"
	"regexp"
	"strings"
	"text/template"
	"unicode"

	"golang.org/x/tools/go/packages"
)

func main() {
	if len(os.Args) != 2 {
		fmt.Fprintln(os.Stderr, "usage: gen [package]")
		os.Exit(1)
	}
	err := generate(os.Args[1])
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
}

type Package struct {
	*packages.Package
	funcs []*Func
}

func NewPackage(pkg *packages.Package) *Package {
	return &Package{pkg, nil}
}

func (pkg Package) PkgPathSegments() []string {
	return strings.Split(pkg.PkgPath, "/")
}

func (pkg Package) Funcs() []*Func {
	if pkg.funcs == nil {
		pkg.funcs = []*Func{}
		scope := pkg.Types.Scope()
		for _, name := range scope.Names() {
			f, ok := scope.Lookup(name).(*types.Func)
			if !ok || !f.Exported() {
				continue
			}
			if f.Type().(*types.Signature).Recv() != nil {
				fmt.Printf("skipping %v because of receiver\n", f)
				continue
			}
			pkg.funcs = append(pkg.funcs, NewFunc(f))
		}
	}
	return pkg.funcs
}

type Func struct {
	*types.Func
	params []*Var
}

func NewFunc(f *types.Func) *Func {
	return &Func{f, nil}
}

func (f Func) InternalName() string {
	internalName := f.Name()
	internalName = string(regexp.MustCompile("[^a-zA-Z0-9]").ReplaceAll([]byte(internalName), []byte{}))
	internalName = string(append([]rune{unicode.ToLower([]rune(internalName)[0])}, []rune(internalName)[1:]...))
	return internalName
}

func (f Func) Params() []*Var {
	if f.params == nil {
		f.params = []*Var{}
		signature := f.Type().(*types.Signature)
		params := signature.Params()
		for i := 0; i < params.Len(); i++ {
			f.params = append(f.params, &Var{params.At(i), false})
		}
		if signature.Variadic() {
			f.params[len(f.params)-1].Variadic = true
		}
	}
	return f.params
}

func (f Func) ParamTypes() string {
	var paramTypes []string
	for _, param := range f.Params() {
		if param.Variadic {
			paramTypes = append(paramTypes, fmt.Sprintf("...%v", param.Type().(*types.Slice).Elem()))
		} else {
			paramTypes = append(paramTypes, param.Type().String())
		}
	}
	return strings.Join(paramTypes, ", ")
}

type Var struct {
	*types.Var
	Variadic bool
}

var fileTemplate = template.Must(template.New("package").Parse(`
package {{.Name}}

import (
	{{.Name}} "{{.PkgPath}}"
	"fmt"
	"reflect"
	"strings"
	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/runtime"
)

var Library *runtime.Library = runtime.MustNewEmptyLibrary([]common.Symbol{
	{{range .PkgPathSegments -}}
	common.Symbol("{{.}}"),
	{{end}}
}, []int{})

var Bindings common.BindingSet

func init() {
	env := common.EmptyEnvironment
	{{range .Funcs -}}
	env = env.MustDefine(common.Symbol("{{.Name}}"), []int{0}, &common.Keyword{common.Function({{.InternalName}})})
	{{end -}}
	Bindings = env.Bindings()
}

{{range .Funcs}}
func {{.InternalName}}(c common.Continuation, args ...common.Datum) (common.EvaluationResult, error) {
	{{if len .Params -}}
	valid := true
	{{range $i, $param := .Params -}}
	{{if $param.Variadic -}}
	var {{$param.Name}} {{$param.Type}}
	for _, arg := range args[{{$i}}:] {
		elem, ok := arg.({{$param.Type.Elem}})
		valid = valid && ok
		{{$param.Name}} = append({{$param.Name}}, elem)
	}
	{{else -}}
	{{$param.Name}}, ok := args[{{$i}}].({{$param.Type}})
	valid = valid && ok
	{{end -}}
	{{end -}}
	if !valid {
		var argTypes []string
		for _, arg := range args {
			argTypes = append(argTypes, reflect.TypeOf(arg).String())
		}
		return common.ErrorC(fmt.Errorf("{{.Name}}: expected ({{.ParamTypes}}) got (%v)", strings.Join(argTypes, ", ")))
	}
	{{end -}}
	{{.FullName}}(
		{{range .Params -}}
		{{if .Variadic -}}
		{{.Name}}...,
		{{else -}}
		{{.Name}},
		{{end -}}
		{{end -}}
	)
	return common.CallC(c, common.Void)
}
{{end}}
`))

func generate(pattern string) error {
	pkg, err := loadPackage(pattern)
	if err != nil {
		return err
	}
	buf := &bytes.Buffer{}
	err = fileTemplate.Execute(buf, NewPackage(pkg))
	if err != nil {
		return err
	}
	fset := token.NewFileSet()
	filename := fmt.Sprintf("%v.go", pkg.Name)
	file, err := parser.ParseFile(fset, filename, buf.String(), 0)
	if err != nil {
		return err
	}
	conf := types.Config{Importer: importer.Default()}
	_, err = conf.Check(fmt.Sprintf("github.com/katsuya94/grime/lib/%v", pkg.PkgPath), fset, []*ast.File{file}, nil)
	if err != nil {
		return err
	}
	filepath, err := goPath()
	if err != nil {
		return err
	}
	filepath = path.Join(filepath, "src/github.com/katsuya94/grime/lib", pkg.PkgPath, fmt.Sprintf("%v.go", pkg.Name))
	err = os.MkdirAll(path.Dir(filepath), 0775)
	if err != nil {
		return err
	}
	f, err := os.OpenFile(filepath, os.O_WRONLY|os.O_CREATE|os.O_TRUNC, 0664)
	if err != nil {
		return err
	}
	err = format.Node(f, fset, file)
	if err != nil {
		return err
	}
	err = f.Close()
	if err != nil {
		return err
	}
	fmt.Printf("wrote %v\n", filepath)
	return nil
}

func goPath() (string, error) {
	cmd := exec.Command("go", "env", "GOPATH")
	b, err := cmd.Output()
	return strings.Trim(string(b), "\n"), err
}

func loadPackage(pattern string) (*packages.Package, error) {
	pkgs, err := packages.Load(&packages.Config{Mode: packages.LoadTypes}, pattern)
	if err != nil {
		return nil, err
	}
	if len(pkgs) != 1 {
		return nil, fmt.Errorf("found %v packages", len(pkgs))
	}
	return pkgs[0], nil
}
