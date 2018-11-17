package main

import (
	"fmt"
	"go/ast"
	"go/format"
	"go/importer"
	"go/parser"
	"go/token"
	"go/types"
	"math"
	"os"
	"strings"

	"golang.org/x/tools/go/packages"
)

func main() {
	err := generate("fmt")
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
}

func generate(pattern string) error {
	pkg, err := loadPackage(pattern)
	if err != nil {
		return err
	}
	src := fmt.Sprintf(`
package %v

import (
	%v "%v"
	"github.com/katsuya94/grime/common"
)
	`, pkg.Name, pkg.Name, pkg.PkgPath)
	scope := pkg.Types.Scope()
	for _, name := range scope.Names() {
		object := scope.Lookup(name)
		if !object.Exported() {
			continue
		}
		switch object := object.(type) {
		case *types.TypeName:
			src += fmt.Sprintf(`
type %v = %v
			`, object.Name(), object.Type())
		case *types.Func:
			signature := object.Type().(*types.Signature)
			src += fmt.Sprintf(`
func %v %v
			`, object.Name(), signature)
		default:
			fmt.Printf("%#v\n", object)
		}
	}
	lines := strings.Split(src, "\n")
	n := len(lines)
	s := int(math.Log10(float64(n))) + 1
	for i, line := range lines {
		fmt.Printf(fmt.Sprintf("%%%dd: %%v\n", s), i, line)
	}
	fset := token.NewFileSet()
	filename := fmt.Sprintf("%v.go", pkg.Name)
	file, err := parser.ParseFile(fset, filename, src, 0)
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
