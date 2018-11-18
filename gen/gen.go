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
	"math"
	"os"
	"regexp"
	"strings"
	"text/template"
	"unicode"

	"golang.org/x/tools/go/packages"
)

func main() {
	err := generate("fmt")
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
}

type fileData struct {
	Name            string
	PkgPath         string
	PkgPathSegments []string
	Funcs           []funcData
}

type funcData struct {
	Name         string
	InternalName string
	Params       []paramData
}

type paramData struct {
	Name string
	Type string
}

var fileTemplate = template.Must(template.New("file").Parse(`
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
	{{range .PkgPathSegments}}
	common.Symbol("{{.}}"),
	{{end}}
}, []int{})

var Bindings common.BindingSet

func init() {
	env := common.EmptyEnvironment
	{{range .Funcs}}
	env = env.MustDefine(common.Symbol("{{.Name}}"), []int{0}, &common.Keyword{common.Function({{.InternalName}})})
	{{end}}
	Bindings = env.Bindings()
}

{{range .Funcs}}
func {{.InternalName}}(c common.Continuation, args ...common.Datum) (common.EvaluationResult, error) {
	valid := true
	{{range $i := .Params}}
	{{.Name}}, ok := args[{{$i}}].({{.Type}})
	valid &&= ok
	{{end}}
	if !valid {
		types := make([]string, len(args))
		for _, arg := range args {
			types = append(types, reflect.TypeOf(arg).String())
		}
		return common.ErrorC(fmt.Errorf("{{.Name}}: expected ({{range $index := .Params}}{{if ne $index 0}}, {{end}}{{.Type}}{{end}}) got %v", strings.Join(types, ", ")))
	}
	{{.Name}}(
		{{range $i := .Params}}
		{{.Name}},
		{{end}}
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
	data := fileData{
		Name:            pkg.Name,
		PkgPath:         pkg.PkgPath,
		PkgPathSegments: strings.Split(pkg.PkgPath, "/"),
	}
	scope := pkg.Types.Scope()
	for _, name := range scope.Names() {
		object := scope.Lookup(name)
		if !object.Exported() {
			continue
		}
		switch object := object.(type) {
		case *types.TypeName:
		case *types.Func:
			signature := object.Type().(*types.Signature)
			if signature.Recv() != nil {
				fmt.Printf("skipping %v because of receiver\n", object)
				continue
			}
			name := object.FullName()
			internalName := object.Name()
			internalName = string(regexp.MustCompile("[^a-zA-Z0-9]").ReplaceAll([]byte(internalName), []byte{}))
			internalName = string(append([]rune{unicode.ToLower([]rune(internalName)[0])}, []rune(internalName)[1:]...))
			params := make([]paramData, signature.Params().Len())
			for i := 0; i < signature.Params().Len(); i++ {
				params = append(params, paramData{
					Name: signature.Params().At(i).Name(),
					Type: signature.Params().At(i).Type().String(),
				})
			}
			data.Funcs = append(data.Funcs, funcData{
				Name:         name,
				InternalName: internalName,
				Params:       params,
			})
		default:
			fmt.Println(object)
		}
	}
	buf := &bytes.Buffer{}
	err = fileTemplate.Execute(buf, data)
	if err != nil {
		return err
	}
	lines := strings.Split(buf.String(), "\n")
	n := len(lines)
	s := int(math.Log10(float64(n))) + 1
	for i, line := range lines {
		fmt.Printf(fmt.Sprintf("%%%dd: %%v\n", s), i, line)
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
	return format.Node(os.Stdout, fset, file)
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
