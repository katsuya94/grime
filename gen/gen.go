package main

import (
	"fmt"
	"go/ast"
	"go/format"
	"go/parser"
	"go/token"
	"go/types"
	"os"

	"golang.org/x/tools/go/packages"
)

func main() {
	err := generate("fmt")
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
	}
}

func generate(patterns ...string) error {
	pkgs, err := packages.Load(&packages.Config{Mode: packages.LoadTypes}, patterns...)
	if err != nil {
		return err
	}
	if len(pkgs) != 1 {
		return fmt.Errorf("found %v packages", len(pkgs))
	}
	pkg := pkgs[0]
	fileSet := token.NewFileSet()
	fileName := fmt.Sprintf("%v.go", pkg.Name)
	src := fmt.Sprintf("package %v", pkg.Name)
	file, err := parser.ParseFile(fileSet, fileName, src, 0)
	if err != nil {
		return err
	}
	_, err = ast.NewPackage(fileSet, map[string]*ast.File{fileName: file}, nil, nil)
	if err != nil {
		return err
	}
	scope := pkg.Types.Scope()
	for _, name := range scope.Names() {
		object := scope.Lookup(name)
		if !object.Exported() {
			continue
		}
		fmt.Println(types.ObjectString(object, nil))
	}
	return format.Node(os.Stdout, fileSet, file)
}
