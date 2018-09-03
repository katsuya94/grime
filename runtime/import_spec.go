package runtime

import (
	"fmt"
	"strconv"

	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/read"
	"github.com/katsuya94/grime/util"
)

var (
	PatternImportSpecFor          = read.MustReadString("(for import-set import-level ...)")[0]
	PatternImportLevelMeta        = read.MustReadString("(meta n)")[0]
	PatternImportLevelRun         = read.MustReadString("run")[0]
	PatternImportLevelExpand      = read.MustReadString("expand")[0]
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
	PatternSubVersionReferenceOr  = read.MustReadString("(or sub-version-reference ...)")[0]
	PatternSubVersionReferenceNot = read.MustReadString("(not sub-version-reference)")[0]
)

type importSpec struct {
	importSet importSet
	levels    []int
}

func newImportSpec(d common.Datum) (importSpec, error) {
	if result, ok, err := util.Match(d, PatternImportSpecFor, map[common.Symbol]common.Binding{
		common.Symbol("for"): nil,
	}); err != nil {
		return importSpec{}, err
	} else if ok {
		iSet, err := newImportSet(result[common.Symbol("import-set")])
		if err != nil {
			return importSpec{}, err
		}
		var levels []int
		for _, d := range result[common.Symbol("import-level")].([]interface{}) {
			level, err := newImportLevel(d)
			if err != nil {
				return importSpec{}, err
			}
			levels = append(levels, level)
		}
		return importSpec{iSet, levels}, nil
	} else {
		iSet, err := newImportSet(d)
		if err != nil {
			return importSpec{}, err
		}
		return importSpec{iSet, []int{0}}, nil
	}
}

func newImportLevel(d common.Datum) (int, error) {
	if result, ok, err := util.Match(d, PatternImportLevelMeta, map[common.Symbol]common.Binding{
		common.Symbol("meta"): nil,
	}); err != nil {
		return 0, err
	} else if ok {
		number, ok := result[common.Symbol("n")].(common.Number)
		if !ok {
			return 0, fmt.Errorf("runtime: malformed import level")
		}
		n, err := strconv.ParseInt(string(number), 10, 0)
		if err != nil {
			return 0, err
		}
		return int(n), nil
	}
	if _, ok, err := util.Match(d, PatternImportLevelRun, map[common.Symbol]common.Binding{
		common.Symbol("run"): nil,
	}); err != nil {
		return 0, err
	} else if ok {
		return 0, nil
	}
	if _, ok, err := util.Match(d, PatternImportLevelExpand, map[common.Symbol]common.Binding{
		common.Symbol("expand"): nil,
	}); err != nil {
		return 0, err
	} else if ok {
		return 1, nil
	}
	return 0, fmt.Errorf("runtime: malformed import level")
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
		return importSetOnly{iSet, ids}, nil
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
		return importSetExcept{iSet, ids}, nil
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
		return importSetPrefix{iSet, id}, nil
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
				return nil, fmt.Errorf("runtime: malformed import set")
			}
		}
		var internalIdentifiers []common.Symbol
		for _, d := range result[common.Symbol("internal")].([]interface{}) {
			if id, ok := d.(common.Symbol); ok {
				internalIdentifiers = append(internalIdentifiers, id)
			} else {
				return nil, fmt.Errorf("runtime: malformed import set")
			}
		}
		var identifierBindings []identifierBinding
		for i := range internalIdentifiers {
			identifierBindings = append(identifierBindings, identifierBinding{internalIdentifiers[i], externalIdentifiers[i]})
		}
		return importSetRename{iSet, identifierBindings}, nil
	}
	return newLibraryReference(d)
}

type importSetOnly struct {
	importSet   importSet
	identifiers []common.Symbol
}
type importSetExcept struct {
	importSet   importSet
	identifiers []common.Symbol
}
type importSetPrefix struct {
	importSet  importSet
	identifier common.Symbol
}
type importSetRename struct {
	importSet          importSet
	identifierBindings []identifierBinding
}

type importSetLibraryReference struct {
	name             []common.Symbol
	versionReference versionReference
}

func newLibraryReference(d common.Datum) (importSetLibraryReference, error) {
	var ref importSetLibraryReference
	result, ok, err := util.Match(d, PatternLibraryReference, map[common.Symbol]common.Binding{})
	if err != nil {
		return importSetLibraryReference{}, err
	} else if !ok {
		return importSetLibraryReference{}, fmt.Errorf("runtime: malformed library reference")
	}
	libraryName := result[common.Symbol("library-name")].([]interface{})
	if len(libraryName) == 0 {
		return importSetLibraryReference{}, fmt.Errorf("runtime: malformed library reference")
	}
	i := 0
	for ; i < len(libraryName); i++ {
		if name, ok := libraryName[i].(common.Symbol); ok {
			ref.name = append(ref.name, name)
		} else {
			break
		}
	}
	ref.versionReference = versionReferenceSubVersionReferences{}
	if i == len(libraryName)-1 {
		vRef, err := newVersionReference(libraryName[i])
		if err != nil {
			return importSetLibraryReference{}, err
		}
		ref.versionReference = vRef
	} else if i < len(libraryName)-1 {
		return importSetLibraryReference{}, fmt.Errorf("runtime: malformed library reference")
	}
	return ref, nil
}

type versionReference interface {
	// TODO implement resolution through an interface method
}

func newVersionReference(d common.Datum) (versionReference, error) {
	if result, ok, err := util.Match(d, PatternVersionReferenceAnd, map[common.Symbol]common.Binding{
		common.Symbol("and"): nil,
	}); err != nil {
		return nil, err
	} else if ok {
		var vRefs []versionReference
		for _, d := range result[common.Symbol("version-reference")].([]interface{}) {
			vRef, err := newVersionReference(d)
			if err != nil {
				return nil, err
			}
			vRefs = append(vRefs, vRef)
		}
		return versionReferenceAnd{vRefs}, nil
	}
	if result, ok, err := util.Match(d, PatternVersionReferenceOr, map[common.Symbol]common.Binding{
		common.Symbol("or"): nil,
	}); err != nil {
		return nil, err
	} else if ok {
		var vRefs []versionReference
		for _, d := range result[common.Symbol("version-reference")].([]interface{}) {
			vRef, err := newVersionReference(d)
			if err != nil {
				return nil, err
			}
			vRefs = append(vRefs, vRef)
		}
		return versionReferenceOr{vRefs}, nil
	}
	if result, ok, err := util.Match(d, PatternVersionReferenceNot, map[common.Symbol]common.Binding{
		common.Symbol("not"): nil,
	}); err != nil {
		return nil, err
	} else if ok {
		vRef, err := newVersionReference(result[common.Symbol("version-reference")])
		if err != nil {
			return nil, err
		}
		return versionReferenceNot{vRef}, nil
	}
	if result, ok, err := util.Match(d, PatternSubVersionReferences, map[common.Symbol]common.Binding{}); err != nil {
		return nil, err
	} else if ok {
		var subVRefs []subVersionReference
		for _, d := range result[common.Symbol("sub-version-reference")].([]interface{}) {
			subVRef, err := newSubVersionReference(d)
			if err != nil {
				return nil, err
			}
			subVRefs = append(subVRefs, subVRef)
		}
		return versionReferenceSubVersionReferences{subVRefs}, nil
	}
	return nil, fmt.Errorf("runtime: malformed version reference")
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
type versionReferenceSubVersionReferences struct {
	subVersionReferences []subVersionReference
}

type subVersionReference interface {
	// TODO implement resolution through an interface method
}

func newSubVersionReference(d common.Datum) (subVersionReference, error) {
	if result, ok, err := util.Match(d, PatternSubVersionReferenceGte, map[common.Symbol]common.Binding{
		common.Symbol(">="): nil,
	}); err != nil {
		return nil, err
	} else if ok {
		subV, err := newSubVersion(result[common.Symbol("sub-version")])
		if err != nil {
			return nil, err
		}
		return subVersionReferenceGte{subV}, nil
	}
	if result, ok, err := util.Match(d, PatternSubVersionReferenceLte, map[common.Symbol]common.Binding{
		common.Symbol("<="): nil,
	}); err != nil {
		return nil, err
	} else if ok {
		subV, err := newSubVersion(result[common.Symbol("sub-version")])
		if err != nil {
			return nil, err
		}
		return subVersionReferenceLte{subV}, nil
	}
	if result, ok, err := util.Match(d, PatternSubVersionReferenceAnd, map[common.Symbol]common.Binding{
		common.Symbol("and"): nil,
	}); err != nil {
		return nil, err
	} else if ok {
		var subVRefs []subVersionReference
		for _, d := range result[common.Symbol("sub-version-reference")].([]interface{}) {
			subVRef, err := newSubVersionReference(d)
			if err != nil {
				return nil, err
			}
			subVRefs = append(subVRefs, subVRef)
		}
		return subVersionReferenceAnd{subVRefs}, nil
	}
	if result, ok, err := util.Match(d, PatternSubVersionReferenceOr, map[common.Symbol]common.Binding{
		common.Symbol("or"): nil,
	}); err != nil {
		return nil, err
	} else if ok {
		var subVRefs []subVersionReference
		for _, d := range result[common.Symbol("sub-version-reference")].([]interface{}) {
			subVRef, err := newSubVersionReference(d)
			if err != nil {
				return nil, err
			}
			subVRefs = append(subVRefs, subVRef)
		}
		return subVersionReferenceOr{subVRefs}, nil
	}
	if result, ok, err := util.Match(d, PatternSubVersionReferenceNot, map[common.Symbol]common.Binding{
		common.Symbol("not"): nil,
	}); err != nil {
		return nil, err
	} else if ok {
		subVRef, err := newSubVersionReference(result[common.Symbol("sub-version-reference")])
		if err != nil {
			return nil, err
		}
		return subVersionReferenceNot{subVRef}, nil
	}
	subV, err := newSubVersion(d)
	if err != nil {
		return nil, err
	}
	return subV, nil
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

func newSubVersion(d common.Datum) (subVersion, error) {
	number, ok := d.(common.Number)
	if !ok {
		return 0, fmt.Errorf("runtime: malformed sub-version")
	}
	n, err := strconv.ParseInt(string(number), 10, 0)
	if err != nil {
		return 0, err
	} else if n < 0 {
		return 0, fmt.Errorf("runtime: malformed sub-version")
	}
	return subVersion(n), nil
}
