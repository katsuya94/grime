package runtime

import (
	"fmt"

	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/read"
	"github.com/katsuya94/grime/util"
)

var (
	PatternLibrary      = util.Pattern(read.MustReadString("(library (library-name ...) (export export-spec ...) (import import-spec ...) body ...)")[0])
	PatternVersion      = util.Pattern(read.MustReadString("(sub-version ...)")[0])
	PatternExportRename = util.Pattern(read.MustReadString("(rename (internal external) ...)")[0])
)

type Library struct {
	name        []common.Symbol
	version     []subVersion
	importSpecs []importSpec
	exportSpecs []identifierBinding
	body        []common.WrappedSyntax
}

func NewLibrary(source common.WrappedSyntax) (*Library, error) {
	var library Library
	result, ok, err := util.MatchSyntax(source, PatternLibrary, map[common.Symbol]common.Location{
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
		result, ok, err := util.MatchSyntax(libraryName[i].(common.WrappedSyntax), PatternVersion, map[common.Symbol]common.Location{})
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
	return &library, nil
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
	if result, ok, err := util.MatchSyntax(d, PatternExportRename, map[common.Symbol]common.Location{
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
