package runtime

import (
	"fmt"
	"strconv"

	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/read"
	"github.com/katsuya94/grime/util"
)

var (
	PatternLibrary                = read.MustReadString("(library (library-name ...) (export export-spec ...) (import import-spec ...) body ...)")[0]
	PatternVersion                = read.MustReadString("(sub-version ...)")[0]
	PatternExportRename           = read.MustReadString("(rename (internal external) ...)")[0]
	PatternImportFor              = read.MustReadString("(for import-set import-level ...)")[0]
	PatternLibraryReference       = read.MustReadString("(library-name ...)")[0]
	PatternImportSetLibrary       = read.MustReadString("(library library-reference)")[0]
	PatternImportSetOnly          = read.MustReadString("(only import-set identifier ...)")[0]
	PatternImportSetExcept        = read.MustReadString("(except import-set identifier ...)")[0]
	PatternImportSetPrefix        = read.MustReadString("(prefix import-set identifier)")[0]
	PatternImportSetRename        = read.MustReadString("(rename import-set (external internal) ...)")[0]
	PatternSubVersionReferences   = read.MustReadString("(sub-version-reference ...)")[0]
	PatternVersionReferenceAnd    = read.MustReadString("(and version-reference ...)")[0]
	PatternVersionReferenceOr     = read.MustReadString("(or version-reference ...)")[0]
	PatternVersionReferenceNot    = read.MustReadString("(not version-reference)")[0]
	PatternSubVersionReferenceGte = read.MustReadString("(>= sub-version)")[0]
	PatternSubVersionReferenceLte = read.MustReadString("(<= sub-version)")[0]
	PatternSubVersionReferenceAnd = read.MustReadString("(and sub-version-reference ...)")[0]
	PatternSubVersionReferencOr   = read.MustReadString("(or sub-version-reference ...)")[0]
	PatternSubVersionReferencNot  = read.MustReadString("(not sub-version-reference)")[0]
)

type Library struct {
	name        []common.Symbol
	version     []subVersion
	importSpecs []*importSpec
	exportSpecs []*identifierBinding
	body        []common.Datum
}

func NewLibrary(source common.Datum) (*Library, error) {
	var library Library
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
			return nil, fmt.Errorf("runtime: malformed library name")
		}
		for _, d := range result[common.Symbol("sub-version")].([]interface{}) {
			if number, ok := d.(common.Number); ok {
				n, err := strconv.ParseInt(string(number), 10, 0)
				if err != nil {
					return nil, fmt.Errorf("runtime: malformed library name: %v", err)
				} else if n < 0 {
					return nil, fmt.Errorf("runtime: malformed library name")
				}
				library.version = append(library.version, subVersion(n))
			} else {
				return nil, fmt.Errorf("runtime: malformed library name")
			}
		}
	} else if i < len(libraryName)-1 {
		return nil, fmt.Errorf("runtime: malformed library name")
	}
	for _, d := range result[common.Symbol("export-spec")].([]interface{}) {
		exportSpecs, err := newExportSpecs(d)
		if err != nil {
			return nil, err
		}
		library.exportSpecs = append(library.exportSpecs, exportSpecs...)
	}
	for _, d := range result[common.Symbol("import-spec")].([]interface{}) {
		importSpec, err := newImportSpec(d)
		if err != nil {
			return nil, err
		}
		library.importSpecs = append(library.importSpecs, importSpec)
	}
	for _, d := range result[common.Symbol("body")].([]interface{}) {
		library.body = append(library.body, d)
	}
	return &library, nil
}

func newExportSpecs(d common.Datum) ([]*identifierBinding, error) {
	if result, ok, err := util.Match(d, PatternExportRename, map[common.Symbol]common.Binding{
		common.Symbol("rename"): nil,
	}); err != nil {
		return nil, err
	} else if ok {
		var internalIdentifiers []common.Symbol
		for _, d := range result[common.Symbol("internal")].([]interface{}) {
			if id, ok := d.(common.Symbol); ok {
				internalIdentifiers = append(internalIdentifiers, id)
			} else {
				return nil, fmt.Errorf("runtime: malformed export spec")
			}
		}
		var externalIdentifiers []common.Symbol
		for _, d := range result[common.Symbol("external")].([]interface{}) {
			if id, ok := d.(common.Symbol); ok {
				externalIdentifiers = append(externalIdentifiers, id)
			} else {
				return nil, fmt.Errorf("runtime: malformed export spec")
			}
		}
		var exportSpecs []*identifierBinding
		for i := range internalIdentifiers {
			exportSpecs = append(exportSpecs, &identifierBinding{internalIdentifiers[i], externalIdentifiers[i]})
		}
		return exportSpecs, nil
	} else if symbol, ok := d.(common.Symbol); ok {
		return []*identifierBinding{{symbol, symbol}}, nil
	} else {
		return nil, fmt.Errorf("runtime: malformed export spec")
	}
}

type importSpec struct {
	importSet importSet
	levels    []int
}

func newImportSpec(common.Datum) (*importSpec, error) {
	return nil, nil
}

type importSet interface {
	// TODO implement resolution through an interface method
}

func newImportSet(d common.Datum) (importSet, error) {
	if result, ok, err := util.Match(d, PatternImportSetLibrary, map[common.Symbol]common.Binding{
		common.Symbol("library"): nil,
	}); err != nil {
		return nil, err
	} else if ok {
		return newLibraryReference(result[common.Symbol("library-reference")])
	}
	if result, ok, err := util.Match(d, PatternImportSetOnly, map[common.Symbol]common.Binding{
		common.Symbol("only"): nil,
	}); err != nil {
		return nil, err
	} else if ok {
		iSet, err := newImportSet(result[common.Symbol("import-set")])
		if err != nil {
			return nil, err
		}
		var ids []common.Symbol
		for _, d := range result[common.Symbol("identifier")].([]interface{}) {
			id, ok := d.(common.Symbol)
			if !ok {
				return nil, fmt.Errorf("runtime: malformed import set")
			}
			ids = append(ids, id)
		}
		return only{iSet, ids}, nil
	}
	if result, ok, err := util.Match(d, PatternImportSetExcept, map[common.Symbol]common.Binding{
		common.Symbol("except"): nil,
	}); err != nil {
		return nil, err
	} else if ok {
		iSet, err := newImportSet(result[common.Symbol("import-set")])
		if err != nil {
			return nil, err
		}
		var ids []common.Symbol
		for _, d := range result[common.Symbol("identifier")].([]interface{}) {
			id, ok := d.(common.Symbol)
			if !ok {
				return nil, fmt.Errorf("runtime: malformed import set")
			}
			ids = append(ids, id)
		}
		return except{iSet, ids}, nil
	}
	if result, ok, err := util.Match(d, PatternImportSetPrefix, map[common.Symbol]common.Binding{
		common.Symbol("prefix"): nil,
	}); err != nil {
		return nil, err
	} else if ok {
		iSet, err := newImportSet(result[common.Symbol("import-set")])
		if err != nil {
			return nil, err
		}
		id, ok := result[common.Symbol("identifier")].(common.Symbol)
		if !ok {
			return nil, fmt.Errorf("runtime: malformed import set")
		}
		return prefix{iSet, id}, nil
	}
	if result, ok, err := util.Match(d, PatternImportSetRename, map[common.Symbol]common.Binding{
		common.Symbol("rename"): nil,
	}); err != nil {
		return nil, err
	} else if ok {
		iSet, err := newImportSet(result[common.Symbol("import-set")])
		if err != nil {
			return nil, err
		}
		var externalIdentifiers []common.Symbol
		for _, d := range result[common.Symbol("external")].([]interface{}) {
			if id, ok := d.(common.Symbol); ok {
				externalIdentifiers = append(externalIdentifiers, id)
			} else {
				return nil, fmt.Errorf("runtime: malformed export spec")
			}
		}
		var internalIdentifiers []common.Symbol
		for _, d := range result[common.Symbol("internal")].([]interface{}) {
			if id, ok := d.(common.Symbol); ok {
				internalIdentifiers = append(internalIdentifiers, id)
			} else {
				return nil, fmt.Errorf("runtime: malformed export spec")
			}
		}
		var identifierBindings []*identifierBinding
		for i := range internalIdentifiers {
			identifierBindings = append(identifierBindings, &identifierBinding{internalIdentifiers[i], externalIdentifiers[i]})
		}
		return rename{iSet, identifierBindings}, nil
	}
	return newLibraryReference(d)
}

type only struct {
	importSet   importSet
	identifiers []common.Symbol
}
type except struct {
	importSet   importSet
	identifiers []common.Symbol
}
type prefix struct {
	importSet  importSet
	identifier common.Symbol
}
type rename struct {
	importSet          importSet
	identifierBindings []*identifierBinding
}

type libraryReference struct {
	name             []common.Symbol
	versionReference versionReference
}

func newLibraryReference(d common.Datum) (*libraryReference, error) {
	var ref libraryReference
	result, ok, err := util.Match(d, PatternLibraryReference, map[common.Symbol]common.Binding{})
	if err != nil {
		return nil, err
	} else if !ok {
		return nil, fmt.Errorf("runtime: malformed library reference")
	}
	libraryName := result[common.Symbol("library-name")].([]interface{})
	if len(libraryName) == 0 {
		return nil, fmt.Errorf("runtime: malformed library reference")
	}
	i := 0
	for ; i < len(libraryName); i++ {
		if name, ok := libraryName[i].(common.Symbol); ok {
			ref.name = append(ref.name, name)
		} else {
			break
		}
	}
	if i == len(libraryName)-1 {
		vRef, err := newVersionReference(libraryName[i])
		if err != nil {
			return nil, err
		} else {
			ref.versionReference = vRef
		}
	} else if i < len(libraryName)-1 {
		return nil, fmt.Errorf("runtime: malformed library reference")
	}
	return &ref, nil
}

type versionReference interface {
	// TODO implement resolution through an interface method
}

func newVersionReference(d common.Datum) (versionReference, error) {

}

type versionReferenceSubVersionReferences struct {
	subVersionReferences []subVersionReference
}
type versionReferenceAnd struct {
	versionReferences []versionReference
}
type versionReferenceOr struct {
	versionReferences []versionReference
}
type versionReferenceNot struct {
	versionReference versionReference
}

type subVersionReference interface {
	// TODO implement resolution through an interface method
}

type subVersionReferenceGte struct {
	subVersion subVersion
}
type subVersionReferenceLte struct {
	subVersion subVersion
}
type subVersionReferenceAnd struct {
	subVersionReferences []subVersionReference
}
type subVersionReferenceOr struct {
	subVersionReferences []subVersionReference
}
type subVersionReferenceNot struct {
	subVersionReference subVersionReference
}

type subVersion int

type identifierBinding struct {
	external common.Symbol
	internal common.Symbol
}

type LibraryInstance struct{}
