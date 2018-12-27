package runtime

import (
	"fmt"
	"io"
	"os"
	"path/filepath"
	"runtime"
	"strings"

	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/read"
)

var (
	PatternLibrary      = common.Pattern(read.MustReadDatum("(library (library-name ...) (export export-spec ...) (import import-spec ...) body ...)"))
	PatternVersion      = common.Pattern(read.MustReadDatum("(sub-version ...)"))
	PatternExportRename = common.Pattern(read.MustReadDatum("(rename (internal external) ...)"))
)

type Library struct {
	name                   []common.Symbol
	version                []subVersion
	importSpecs            []importSpec
	exportSpecs            []identifierBinding
	body                   []common.WrappedSyntax
	nullSourceLocationTree common.SourceLocationTree
}

func NewLibrary(source common.WrappedSyntax) (*Library, error) {
	var library Library
	result, ok, err := common.MatchSyntax(source, PatternLibrary, map[common.Symbol]common.Location{
		common.Symbol("library"): nil,
		common.Symbol("export"):  nil,
		common.Symbol("import"):  nil,
	})
	if err != nil {
		return nil, err
	} else if !ok {
		return nil, fmt.Errorf("runtime: malformed library")
	}
	libraryName := result[common.Symbol("library-name")].([]interface{})
	if len(libraryName) == 0 {
		return nil, fmt.Errorf("runtime: malformed library")
	}
	i := 0
	for ; i < len(libraryName); i++ {
		if name, ok := libraryName[i].(common.WrappedSyntax).Datum().(common.Symbol); ok {
			library.name = append(library.name, name)
		} else {
			break
		}
	}
	if i == len(libraryName)-1 {
		result, ok, err := common.MatchSyntax(libraryName[i].(common.WrappedSyntax), PatternVersion, map[common.Symbol]common.Location{})
		if err != nil {
			return nil, err
		} else if !ok {
			return nil, fmt.Errorf("runtime: malformed library name")
		}
		for _, d := range result[common.Symbol("sub-version")].([]interface{}) {
			subV, err := newSubVersion(d.(common.WrappedSyntax))
			if err != nil {
				return nil, err
			}
			library.version = append(library.version, subV)
		}
	} else if i < len(libraryName)-1 {
		return nil, fmt.Errorf("runtime: malformed library name")
	}
	for _, d := range result[common.Symbol("export-spec")].([]interface{}) {
		exportSpecs, err := newExportSpecs(d.(common.WrappedSyntax))
		if err != nil {
			return nil, err
		}
		library.exportSpecs = append(library.exportSpecs, exportSpecs...)
	}
	for _, d := range result[common.Symbol("import-spec")].([]interface{}) {
		importSpec, err := newImportSpec(d.(common.WrappedSyntax))
		if err != nil {
			return nil, err
		}
		library.importSpecs = append(library.importSpecs, importSpec)
	}
	for _, d := range result[common.Symbol("body")].([]interface{}) {
		library.body = append(library.body, d.(common.WrappedSyntax))
	}
	null := source
	for null.Datum() != common.Null {
		null = null.PushDown().(common.Pair).Rest.(common.WrappedSyntax)
	}
	library.nullSourceLocationTree = *null.SourceLocationTree()
	return &library, nil
}

func MustNewLibraryFromReader(name string, r io.Reader) *Library {
	syntaxes, _, err := read.Read(name, r)
	if err != nil {
		panic(fmt.Sprintf("failed to load %v: %v", name, err))
	}
	if len(syntaxes) != 1 {
		panic(fmt.Sprintf("failed to load %v: found %v data", name, len(syntaxes)))
	}
	return MustNewLibrary(syntaxes[0])
}

func MustNewLibraryFromString(name string, src string) *Library {
	return MustNewLibraryFromReader(name, strings.NewReader(src))
}

func MustNewLibraryFromFile(name string) *Library {
	_, filename, _, ok := runtime.Caller(1)
	if !ok {
		panic(fmt.Sprintf("failed to load %v", name))
	}
	sourcePath := filepath.Join(filepath.Dir(filename), fmt.Sprintf("%v.scm", name))
	f, err := os.Open(sourcePath)
	if err != nil {
		panic(fmt.Sprintf("failed to load %v: %v", name, err))
	}
	return MustNewLibraryFromReader(sourcePath, f)
}

func MustNewLibrary(source common.WrappedSyntax) *Library {
	library, err := NewLibrary(source)
	if err != nil {
		panic(err)
	}
	return library
}

func NewEmptyLibrary(name []common.Symbol, version []int) (*Library, error) {
	var library Library
	library.name = name
	for _, n := range version {
		subV, err := newSubVersionInt(n)
		if err != nil {
			return nil, err
		}
		library.version = append(library.version, subV)
	}
	return &library, nil
}

func MustNewEmptyLibrary(name []common.Symbol, version []int) *Library {
	library, err := NewEmptyLibrary(name, version)
	if err != nil {
		panic(err)
	}
	return library
}

func (l *Library) Name() []common.Symbol {
	return l.name
}

func newExportSpecs(d common.WrappedSyntax) ([]identifierBinding, error) {
	if result, ok, err := common.MatchSyntax(d, PatternExportRename, map[common.Symbol]common.Location{
		common.Symbol("rename"): nil,
	}); err != nil {
		return nil, err
	} else if ok {
		var internalIdentifiers []common.Symbol
		for _, d := range result[common.Symbol("internal")].([]interface{}) {
			if id, ok := d.(common.WrappedSyntax).Datum().(common.Symbol); ok {
				internalIdentifiers = append(internalIdentifiers, id)
			} else {
				return nil, fmt.Errorf("runtime: malformed export spec")
			}
		}
		var externalIdentifiers []common.Symbol
		for _, d := range result[common.Symbol("external")].([]interface{}) {
			if id, ok := d.(common.WrappedSyntax).Datum().(common.Symbol); ok {
				externalIdentifiers = append(externalIdentifiers, id)
			} else {
				return nil, fmt.Errorf("runtime: malformed export spec")
			}
		}
		var exportSpecs []identifierBinding
		for i := range internalIdentifiers {
			exportSpecs = append(exportSpecs, identifierBinding{internalIdentifiers[i], externalIdentifiers[i]})
		}
		return exportSpecs, nil
	}
	symbol, ok := d.Datum().(common.Symbol)
	if !ok {
		return nil, fmt.Errorf("runtime: malformed export spec")
	}
	return []identifierBinding{{symbol, symbol}}, nil
}

func sameName(n1 []common.Symbol, n2 []common.Symbol) bool {
	if len(n1) != len(n2) {
		return false
	}
	for i := range n1 {
		if n1[i] != n2[i] {
			return false
		}
	}
	return true
}
