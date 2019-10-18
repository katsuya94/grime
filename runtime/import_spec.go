package runtime

import (
	"fmt"
	"strconv"

	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/read"
)

var (
	PatternImportSpecFor          = common.MustCompileSimplePattern(read.MustReadDatum("(for import-set import-level ...)"), common.Symbol("for"))
	PatternImportLevelMeta        = common.MustCompileSimplePattern(read.MustReadDatum("(meta n)"), common.Symbol("meta"))
	PatternImportLevelRun         = common.MustCompileSimplePattern(read.MustReadDatum("run"), common.Symbol("run"))
	PatternImportLevelExpand      = common.MustCompileSimplePattern(read.MustReadDatum("expand"), common.Symbol("expand"))
	PatternLibraryReference       = common.MustCompileSimplePattern(read.MustReadDatum("(library-name ...)"))
	PatternImportSetLibrary       = common.MustCompileSimplePattern(read.MustReadDatum("(library library-reference)"), common.Symbol("library"))
	PatternImportSetOnly          = common.MustCompileSimplePattern(read.MustReadDatum("(only import-set identifier ...)"), common.Symbol("only"))
	PatternImportSetExcept        = common.MustCompileSimplePattern(read.MustReadDatum("(except import-set identifier ...)"), common.Symbol("except"))
	PatternImportSetPrefix        = common.MustCompileSimplePattern(read.MustReadDatum("(prefix import-set identifier)"), common.Symbol("prefix"))
	PatternImportSetRename        = common.MustCompileSimplePattern(read.MustReadDatum("(rename import-set (external internal) ...)"), common.Symbol("rename"))
	PatternSubVersionReferences   = common.MustCompileSimplePattern(read.MustReadDatum("(sub-version-reference ...)"))
	PatternVersionReferenceAnd    = common.MustCompileSimplePattern(read.MustReadDatum("(and version-reference ...)"), common.Symbol("and"))
	PatternVersionReferenceOr     = common.MustCompileSimplePattern(read.MustReadDatum("(or version-reference ...)"), common.Symbol("or"))
	PatternVersionReferenceNot    = common.MustCompileSimplePattern(read.MustReadDatum("(not version-reference)"), common.Symbol("not"))
	PatternSubVersionReferenceGte = common.MustCompileSimplePattern(read.MustReadDatum("(>= sub-version)"), common.Symbol(">="))
	PatternSubVersionReferenceLte = common.MustCompileSimplePattern(read.MustReadDatum("(<= sub-version)"), common.Symbol("<="))
	PatternSubVersionReferenceAnd = common.MustCompileSimplePattern(read.MustReadDatum("(and sub-version-reference ...)"), common.Symbol("and"))
	PatternSubVersionReferenceOr  = common.MustCompileSimplePattern(read.MustReadDatum("(or sub-version-reference ...)"), common.Symbol("or"))
	PatternSubVersionReferenceNot = common.MustCompileSimplePattern(read.MustReadDatum("(not sub-version-reference)"), common.Symbol("not"))
)

type importSpec struct {
	importSet importSet
	levels    []int
}

func newImportSpec(d common.Syntax) (importSpec, error) {
	if result, ok := PatternImportSpecFor.Match(d); ok {
		iSet, err := newImportSet(result[common.Symbol("import-set")].(common.Syntax))
		if err != nil {
			return importSpec{}, err
		}
		var levels []int
		for _, d := range result[common.Symbol("import-level")].([]interface{}) {
			level, err := newImportLevel(d.(common.Syntax))
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

type importSpecResolution struct {
	identifierSpec identifierSpec
	levels         []int
}

func (spec importSpec) resolve(library Library) (importSpecResolution, bool) {
	importSetResolution, ok := spec.importSet.resolve(library)
	return importSpecResolution{importSetResolution, spec.levels}, ok
}

func (spec importSpec) libraryName() []common.Symbol {
	return spec.importSet.libraryName()
}

func newImportLevel(d common.Syntax) (int, error) {
	if result, ok := PatternImportLevelMeta.Match(d); ok {
		number, ok := result[common.Symbol("n")].(common.Syntax).Unwrap().(common.Number)
		if !ok {
			return 0, fmt.Errorf("runtime: malformed import level")
		}
		n, err := strconv.ParseInt(string(number), 10, 0)
		if err != nil {
			return 0, err
		}
		return int(n), nil
	}
	if _, ok := PatternImportLevelRun.Match(d); ok {
		return 0, nil
	}
	if _, ok := PatternImportLevelExpand.Match(d); ok {
		return 1, nil
	}
	return 0, fmt.Errorf("runtime: malformed import level")
}

type importSet interface {
	resolve(Library) (identifierSpec, bool)
	libraryName() []common.Symbol
}

func newImportSet(d common.Syntax) (importSet, error) {
	if result, ok := PatternImportSetLibrary.Match(d); ok {
		return newLibraryReference(result[common.Symbol("library-reference")].(common.Syntax))
	}
	if result, ok := PatternImportSetOnly.Match(d); ok {
		iSet, err := newImportSet(result[common.Symbol("import-set")].(common.Syntax))
		if err != nil {
			return nil, err
		}
		var ids []common.Symbol
		for _, d := range result[common.Symbol("identifier")].([]interface{}) {
			id, ok := d.(common.Syntax).Unwrap().(common.Symbol)
			if !ok {
				return nil, fmt.Errorf("runtime: malformed import set")
			}
			ids = append(ids, id)
		}
		return importSetOnly{iSet, ids}, nil
	}
	if result, ok := PatternImportSetExcept.Match(d); ok {
		iSet, err := newImportSet(result[common.Symbol("import-set")].(common.Syntax))
		if err != nil {
			return nil, err
		}
		var ids []common.Symbol
		for _, d := range result[common.Symbol("identifier")].([]interface{}) {
			id, ok := d.(common.Syntax).Unwrap().(common.Symbol)
			if !ok {
				return nil, fmt.Errorf("runtime: malformed import set")
			}
			ids = append(ids, id)
		}
		return importSetExcept{iSet, ids}, nil
	}
	if result, ok := PatternImportSetPrefix.Match(d); ok {
		iSet, err := newImportSet(result[common.Symbol("import-set")].(common.Syntax))
		if err != nil {
			return nil, err
		}
		id, ok := result[common.Symbol("identifier")].(common.Syntax).Unwrap().(common.Symbol)
		if !ok {
			return nil, fmt.Errorf("runtime: malformed import set")
		}
		return importSetPrefix{iSet, id}, nil
	}
	if result, ok := PatternImportSetRename.Match(d); ok {
		iSet, err := newImportSet(result[common.Symbol("import-set")].(common.Syntax))
		if err != nil {
			return nil, err
		}
		var externalIdentifiers []common.Symbol
		for _, d := range result[common.Symbol("external")].([]interface{}) {
			if id, ok := d.(common.Syntax).Unwrap().(common.Symbol); ok {
				externalIdentifiers = append(externalIdentifiers, id)
			} else {
				return nil, fmt.Errorf("runtime: malformed import set")
			}
		}
		var internalIdentifiers []common.Symbol
		for _, d := range result[common.Symbol("internal")].([]interface{}) {
			if id, ok := d.(common.Syntax).Unwrap().(common.Symbol); ok {
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

func (set importSetOnly) resolve(library Library) (identifierSpec, bool) {
	subSpec, ok := set.importSet.resolve(library)
	return identifierSpecOnly{
		subSpec,
		set.identifiers,
	}, ok
}

func (set importSetOnly) libraryName() []common.Symbol {
	return set.importSet.libraryName()
}

type importSetExcept struct {
	importSet   importSet
	identifiers []common.Symbol
}

func (set importSetExcept) resolve(library Library) (identifierSpec, bool) {
	subSpec, ok := set.importSet.resolve(library)
	return identifierSpecExcept{
		subSpec,
		set.identifiers,
	}, ok
}

func (set importSetExcept) libraryName() []common.Symbol {
	return set.importSet.libraryName()
}

type importSetPrefix struct {
	importSet  importSet
	identifier common.Symbol
}

func (set importSetPrefix) resolve(library Library) (identifierSpec, bool) {
	subSpec, ok := set.importSet.resolve(library)
	return identifierSpecPrefix{
		subSpec,
		set.identifier,
	}, ok
}

func (set importSetPrefix) libraryName() []common.Symbol {
	return set.importSet.libraryName()
}

type importSetRename struct {
	importSet          importSet
	identifierBindings []identifierBinding
}

func (set importSetRename) resolve(library Library) (identifierSpec, bool) {
	subSpec, ok := set.importSet.resolve(library)
	return identifierSpecRename{
		subSpec,
		set.identifierBindings,
	}, ok
}

func (set importSetRename) libraryName() []common.Symbol {
	return set.importSet.libraryName()
}

type importSetLibraryReference struct {
	name             []common.Symbol
	versionReference versionReference
}

func newLibraryReference(d common.Syntax) (importSetLibraryReference, error) {
	var ref importSetLibraryReference
	result, ok := PatternLibraryReference.Match(d)
	if !ok {
		return importSetLibraryReference{}, fmt.Errorf("runtime: malformed library reference")
	}
	libraryName := result[common.Symbol("library-name")].([]interface{})
	if len(libraryName) == 0 {
		return importSetLibraryReference{}, fmt.Errorf("runtime: malformed library reference")
	}
	i := 0
	for ; i < len(libraryName); i++ {
		if name, ok := libraryName[i].(common.Syntax).Unwrap().(common.Symbol); ok {
			ref.name = append(ref.name, name)
		} else {
			break
		}
	}
	ref.versionReference = versionReferenceSubVersionReferences{}
	if i == len(libraryName)-1 {
		vRef, err := newVersionReference(libraryName[i].(common.Syntax))
		if err != nil {
			return importSetLibraryReference{}, err
		}
		ref.versionReference = vRef
	} else if i < len(libraryName)-1 {
		return importSetLibraryReference{}, fmt.Errorf("runtime: malformed library reference")
	}
	return ref, nil
}

func (set importSetLibraryReference) resolve(library Library) (identifierSpec, bool) {
	ok := sameName(set.name, library.name) && set.versionReference.resolve(library)
	return identifierSpecAll{}, ok
}

func (set importSetLibraryReference) libraryName() []common.Symbol {
	return set.name
}

type versionReference interface {
	resolve(Library) bool
}

func newVersionReference(d common.Syntax) (versionReference, error) {
	if result, ok := PatternVersionReferenceAnd.Match(d); ok {
		var vRefs []versionReference
		for _, d := range result[common.Symbol("version-reference")].([]interface{}) {
			vRef, err := newVersionReference(d.(common.Syntax))
			if err != nil {
				return nil, err
			}
			vRefs = append(vRefs, vRef)
		}
		return versionReferenceAnd{vRefs}, nil
	}
	if result, ok := PatternVersionReferenceOr.Match(d); ok {
		var vRefs []versionReference
		for _, d := range result[common.Symbol("version-reference")].([]interface{}) {
			vRef, err := newVersionReference(d.(common.Syntax))
			if err != nil {
				return nil, err
			}
			vRefs = append(vRefs, vRef)
		}
		return versionReferenceOr{vRefs}, nil
	}
	if result, ok := PatternVersionReferenceNot.Match(d); ok {
		vRef, err := newVersionReference(result[common.Symbol("version-reference")].(common.Syntax))
		if err != nil {
			return nil, err
		}
		return versionReferenceNot{vRef}, nil
	}
	if result, ok := PatternSubVersionReferences.Match(d); ok {
		var subVRefs []subVersionReference
		for _, d := range result[common.Symbol("sub-version-reference")].([]interface{}) {
			subVRef, err := newSubVersionReference(d.(common.Syntax))
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

func (ref versionReferenceAnd) resolve(library Library) bool {
	for _, subRef := range ref.versionReferences {
		if !subRef.resolve(library) {
			return false
		}
	}
	return true
}

type versionReferenceOr struct {
	versionReferences []versionReference
}

func (ref versionReferenceOr) resolve(library Library) bool {
	for _, subRef := range ref.versionReferences {
		if subRef.resolve(library) {
			return true
		}
	}
	return false
}

type versionReferenceNot struct {
	versionReference versionReference
}

func (ref versionReferenceNot) resolve(library Library) bool {
	return !ref.versionReference.resolve(library)
}

type versionReferenceSubVersionReferences struct {
	subVersionReferences []subVersionReference
}

func (ref versionReferenceSubVersionReferences) resolve(library Library) bool {
	if len(ref.subVersionReferences) > len(library.version) {
		return false
	}
	for i := range ref.subVersionReferences {
		if !ref.subVersionReferences[i].resolve(library.version[i]) {
			return false
		}
	}
	return true
}

type subVersionReference interface {
	resolve(subVersion) bool
}

func newSubVersionReference(d common.Syntax) (subVersionReference, error) {
	if result, ok := PatternSubVersionReferenceGte.Match(d); ok {
		subV, err := newSubVersion(result[common.Symbol("sub-version")].(common.Syntax))
		if err != nil {
			return nil, err
		}
		return subVersionReferenceGte{subV}, nil
	}
	if result, ok := PatternSubVersionReferenceLte.Match(d); ok {
		subV, err := newSubVersion(result[common.Symbol("sub-version")].(common.Syntax))
		if err != nil {
			return nil, err
		}
		return subVersionReferenceLte{subV}, nil
	}
	if result, ok := PatternSubVersionReferenceAnd.Match(d); ok {
		var subVRefs []subVersionReference
		for _, d := range result[common.Symbol("sub-version-reference")].([]interface{}) {
			subVRef, err := newSubVersionReference(d.(common.Syntax))
			if err != nil {
				return nil, err
			}
			subVRefs = append(subVRefs, subVRef)
		}
		return subVersionReferenceAnd{subVRefs}, nil
	}
	if result, ok := PatternSubVersionReferenceOr.Match(d); ok {
		var subVRefs []subVersionReference
		for _, d := range result[common.Symbol("sub-version-reference")].([]interface{}) {
			subVRef, err := newSubVersionReference(d.(common.Syntax))
			if err != nil {
				return nil, err
			}
			subVRefs = append(subVRefs, subVRef)
		}
		return subVersionReferenceOr{subVRefs}, nil
	}
	if result, ok := PatternSubVersionReferenceNot.Match(d); ok {
		subVRef, err := newSubVersionReference(result[common.Symbol("sub-version-reference")].(common.Syntax))
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

func (ref subVersionReferenceGte) resolve(subV subVersion) bool {
	return subV >= ref.subVersion
}

type subVersionReferenceLte struct {
	subVersion subVersion
}

func (ref subVersionReferenceLte) resolve(subV subVersion) bool {
	return subV <= ref.subVersion
}

type subVersionReferenceAnd struct {
	subVersionReferences []subVersionReference
}

func (ref subVersionReferenceAnd) resolve(subV subVersion) bool {
	for _, subRef := range ref.subVersionReferences {
		if !subRef.resolve(subV) {
			return false
		}
	}
	return true
}

type subVersionReferenceOr struct {
	subVersionReferences []subVersionReference
}

func (ref subVersionReferenceOr) resolve(subV subVersion) bool {
	for _, subRef := range ref.subVersionReferences {
		if subRef.resolve(subV) {
			return true
		}
	}
	return false
}

type subVersionReferenceNot struct {
	subVersionReference subVersionReference
}

func (ref subVersionReferenceNot) resolve(subV subVersion) bool {
	return !ref.subVersionReference.resolve(subV)
}

type subVersion int

func (ref subVersion) resolve(subV subVersion) bool {
	return subV == ref
}

func newSubVersion(d common.Syntax) (subVersion, error) {
	number, ok := d.Unwrap().(common.Number)
	if !ok {
		return 0, fmt.Errorf("runtime: malformed sub-version")
	}
	n, err := strconv.ParseInt(string(number), 10, 0)
	if err != nil {
		return 0, err
	}
	return newSubVersionInt(int(n))
}

func newSubVersionInt(n int) (subVersion, error) {
	if n < 0 {
		return 0, fmt.Errorf("runtime: malformed sub-version")
	}
	return subVersion(n), nil
}
