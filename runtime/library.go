package runtime

import (
	"fmt"
	"strconv"

	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/read"
	"github.com/katsuya94/grime/util"
)

var (
	PatternLibrary = read.MustReadString("(library (library-name ...) (export export-spec ...) (import import-spec ...) body ...)")[0]
	PatternVersion = read.MustReadString("(sub-version ...)")[0]
)

type Library struct {
	name        []common.Symbol
	version     []int
	importSpecs []*importSpec
	exportSpecs []*exportSpec
	body        []common.Datum
}

func NewLibrary(source common.Datum) (*Library, error) {
	library := &Library{}
	result, ok, err := util.Match(source, PatternLibrary, map[common.Symbol]common.Binding{
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
		if name, ok := libraryName[i].(common.Symbol); ok {
			library.name = append(library.name, name)
		} else {
			break
		}
	}
	if i == len(libraryName)-1 {
		result, ok, err := util.Match(libraryName[i], PatternVersion, map[common.Symbol]common.Binding{})
		if err != nil {
			return nil, err
		} else if !ok {
			return nil, fmt.Errorf("runtime: malformed library")
		}
		for _, stx := range result[common.Symbol("sub-version")].([]interface{}) {
			if n, ok := stx.(common.Number); ok {
				subVersion, err := strconv.ParseInt(string(n), 10, 0)
				if err != nil {
					return nil, fmt.Errorf("runtime: malformed library: %v", err)
				} else if subVersion < 0 {
					return nil, fmt.Errorf("runtime: malformed library")
				}
				library.version = append(library.version, int(subVersion))
			} else {
				return nil, fmt.Errorf("runtime: malformed library")
			}
		}
	} else if i < len(libraryName)-1 {
		return nil, fmt.Errorf("runtime: malformed library")
	}
	for _, stx := range result[common.Symbol("export-spec")].([]interface{}) {
		exportSpec, err := newExportSpec(stx)
		if err != nil {
			return nil, err
		}
		library.exportSpecs = append(library.exportSpecs, exportSpec)
	}
	for _, stx := range result[common.Symbol("import-spec")].([]interface{}) {
		importSpec, err := newImportSpec(stx)
		if err != nil {
			return nil, err
		}
		library.importSpecs = append(library.importSpecs, importSpec)
	}
	for _, stx := range result[common.Symbol("body")].([]interface{}) {
		library.body = append(library.body, stx)
	}
	return library, nil
}

type exportSpec struct{}

func newExportSpec(common.Datum) (*exportSpec, error) {
	return nil, nil
}

type importSpec struct{}

func newImportSpec(common.Datum) (*importSpec, error) {
	return nil, nil
}

type LibraryInstance struct{}
