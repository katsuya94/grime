package gen

import (
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
	"strings"

	"golang.org/x/tools/go/packages"
)

var goPath string

func init() {
	cmd := exec.Command("go", "env", "GOPATH")
	b, err := cmd.Output()
	if err != nil {
		panic(err)
	}
	goPath = strings.Trim(string(b), "\n")
}

func Generate(pattern string) error {
	ok, err := generateGrimePackage(pattern)
	if err != nil {
		return err
	} else if ok {
		return nil
	}
	ok, err = generateGoPackage(pattern)
	if err != nil {
		return err
	} else if ok {
		return nil
	}
	return fmt.Errorf("no matching package found")
}

func generatedPackagePath(pkgPath string) string {
	return fmt.Sprintf("github.com/katsuya94/grime/lib/%v", pkgPath)
}

func loadPackage(pattern string) (*packages.Package, error) {
	pkgs, err := packages.Load(&packages.Config{Mode: packages.LoadTypes}, pattern)
	if err != nil {
		return nil, err
	}
	if len(pkgs) == 0 {
		return nil, nil
	} else if len(pkgs) == 1 {
		return pkgs[0], nil
	}
	return nil, fmt.Errorf("found %v packages", len(pkgs))
}

func parseCheckWrite(filename string, pkgPath string, src string) error {
	fset := token.NewFileSet()
	file, err := parser.ParseFile(fset, filename, src, parser.ParseComments)
	if err != nil {
		fmt.Fprintln(os.Stderr, src)
		return err
	}
	conf := types.Config{Importer: importer.Default()}
	_, err = conf.Check(pkgPath, fset, []*ast.File{file}, nil)
	if err != nil {
		fmt.Fprintln(os.Stderr, src)
		return err
	}
	filepath := path.Join(goPath, "src", pkgPath, filename)
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
