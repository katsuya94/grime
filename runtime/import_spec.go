package runtime

import (
	"fmt"
	"strconv"

	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/read"
	"github.com/katsuya94/grime/util"
)

var (
	PatternImportSpecFor          = util.Pattern(read.MustReadString("(for import-set import-level ...)")[0])
	PatternImportLevelMeta        = util.Pattern(read.MustReadString("(meta n)")[0])
	PatternImportLevelRun         = util.Pattern(read.MustReadString("run")[0])
	PatternImportLevelExpand      = util.Pattern(read.MustReadString("expand")[0])
	PatternLibraryReference       = util.Pattern(read.MustReadString("(library-name ...)")[0])
	PatternImportSetLibrary       = util.Pattern(read.MustReadString("(library library-reference)")[0])
	PatternImportSetOnly          = util.Pattern(read.MustReadString("(only import-set identifier ...)")[0])
	PatternImportSetExcept        = util.Pattern(read.MustReadString("(except import-set identifier ...)")[0])
	PatternImportSetPrefix        = util.Pattern(read.MustReadString("(prefix import-set identifier)")[0])
	PatternImportSetRename        = util.Pattern(read.MustReadString("(rename import-set (external internal) ...)")[0])
	PatternSubVersionReferences   = util.Pattern(read.MustReadString("(sub-version-reference ...)")[0])
	PatternVersionReferenceAnd    = util.Pattern(read.MustReadString("(and version-reference ...)")[0])
	PatternVersionReferenceOr     = util.Pattern(read.MustReadString("(or version-reference ...)")[0])
	PatternVersionReferenceNot    = util.Pattern(read.MustReadString("(not version-reference)")[0])
	PatternSubVersionReferenceGte = util.Pattern(read.MustReadString("(>= sub-version)")[0])
	PatternSubVersionReferenceLte = util.Pattern(read.MustReadString("(<= sub-version)")[0])
	PatternSubVersionReferenceAnd = util.Pattern(read.MustReadString("(and sub-version-reference ...)")[0])
	PatternSubVersionReferenceOr  = util.Pattern(read.MustReadString("(or sub-version-reference ...)")[0])
	PatternSubVersionReferenceNot = util.Pattern(read.MustReadString("(not sub-version-reference)")[0])
)

type importSpec struct {
	importSet importSet
	levels    []int
}

func newImportSpec(d common.WrappedSyntax) (importSpec, error) {
	if result, ok, err := util.MatchSyntax(d, PatternImportSpecFor, map[common.Symbol]common.Location{
		common.Symbol("for"): nil,
	}); err != nil {
		return importSpec{}, err
	} else if ok {
		iSet, err := newImportSet(result[common.Symbol("import-set")].(common.WrappedSyntax))
		if err != nil {
			return importSpec{}, err
		}
		var levels []int
		for _, d := range result[common.Symbol("import-level")].([]interface{}) {
			level, err := newImportLevel(d.(common.WrappedSyntax))
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
	importSetResolution
	levels []int
}

func (spec importSpec) resolve(library *Library) (importSpecResolution, bool) {
	importSetResolution, ok := spec.importSet.resolve(library)
	return importSpecResolution{importSetResolution, spec.levels}, ok
}

func (spec importSpec) libraryName() []common.Symbol {
	return spec.importSet.libraryName()
}

func newImportLevel(d common.WrappedSyntax) (int, error) {
	if result, ok, err := util.MatchSyntax(d, PatternImportLevelMeta, map[common.Symbol]common.Location{
		common.Symbol("meta"): nil,
	}); err != nil {
		return 0, err
	} else if ok {
		number, ok := result[common.Symbol("n")].(common.WrappedSyntax).Datum().(common.Number)
		if !ok {
			return 0, fmt.Errorf("runtime: malformed import level")
		}
		n, err := strconv.ParseInt(string(number), 10, 0)
		if err != nil {
			return 0, err
		}
		return int(n), nil
	}
	if _, ok, err := util.MatchSyntax(d, PatternImportLevelRun, map[common.Symbol]common.Location{
		common.Symbol("run"): nil,
	}); err != nil {
		return 0, err
	} else if ok {
		return 0, nil
	}
	if _, ok, err := util.MatchSyntax(d, PatternImportLevelExpand, map[common.Symbol]common.Location{
		common.Symbol("expand"): nil,
	}); err != nil {
		return 0, err
	} else if ok {
		return 1, nil
	}
	return 0, fmt.Errorf("runtime: malformed import level")
}

type importSet interface {
	resolve(*Library) (importSetResolution, bool)
	libraryName() []common.Symbol
}

func newImportSet(d common.WrappedSyntax) (importSet, error) {
	if result, ok, err := util.MatchSyntax(d, PatternImportSetLibrary, map[common.Symbol]common.Location{
		common.Symbol("library"): nil,
	}); err != nil {
		return nil, err
	} else if ok {
		return newLibraryReference(result[common.Symbol("library-reference")].(common.WrappedSyntax))
	}
	if result, ok, err := util.MatchSyntax(d, PatternImportSetOnly, map[common.Symbol]common.Location{
		common.Symbol("only"): nil,
	}); err != nil {
		return nil, err
	} else if ok {
		iSet, err := newImportSet(result[common.Symbol("import-set")].(common.WrappedSyntax))
		if err != nil {
			return nil, err
		}
		var ids []common.Symbol
		for _, d := range result[common.Symbol("identifier")].([]interface{}) {
			id, ok := d.(common.WrappedSyntax).Datum().(common.Symbol)
			if !ok {
				return nil, fmt.Errorf("runtime: malformed import set")
			}
			ids = append(ids, id)
		}
		return importSetOnly{iSet, ids}, nil
	}
	if result, ok, err := util.MatchSyntax(d, PatternImportSetExcept, map[common.Symbol]common.Location{
		common.Symbol("except"): nil,
	}); err != nil {
		return nil, err
	} else if ok {
		iSet, err := newImportSet(result[common.Symbol("import-set")].(common.WrappedSyntax))
		if err != nil {
			return nil, err
		}
		var ids []common.Symbol
		for _, d := range result[common.Symbol("identifier")].([]interface{}) {
			id, ok := d.(common.WrappedSyntax).Datum().(common.Symbol)
			if !ok {
				return nil, fmt.Errorf("runtime: malformed import set")
			}
			ids = append(ids, id)
		}
		return importSetExcept{iSet, ids}, nil
	}
	if result, ok, err := util.MatchSyntax(d, PatternImportSetPrefix, map[common.Symbol]common.Location{
		common.Symbol("prefix"): nil,
	}); err != nil {
		return nil, err
	} else if ok {
		iSet, err := newImportSet(result[common.Symbol("import-set")].(common.WrappedSyntax))
		if err != nil {
			return nil, err
		}
		id, ok := result[common.Symbol("identifier")].(common.WrappedSyntax).Datum().(common.Symbol)
		if !ok {
			return nil, fmt.Errorf("runtime: malformed import set")
		}
		return importSetPrefix{iSet, id}, nil
	}
	if result, ok, err := util.MatchSyntax(d, PatternImportSetRename, map[common.Symbol]common.Location{
		common.Symbol("rename"): nil,
	}); err != nil {
		return nil, err
	} else if ok {
		iSet, err := newImportSet(result[common.Symbol("import-set")].(common.WrappedSyntax))
		if err != nil {
			return nil, err
		}
		var externalIdentifiers []common.Symbol
		for _, d := range result[common.Symbol("external")].([]interface{}) {
			if id, ok := d.(common.WrappedSyntax).Datum().(common.Symbol); ok {
				externalIdentifiers = append(externalIdentifiers, id)
			} else {
				return nil, fmt.Errorf("runtime: malformed import set")
			}
		}
		var internalIdentifiers []common.Symbol
		for _, d := range result[common.Symbol("internal")].([]interface{}) {
			if id, ok := d.(common.WrappedSyntax).Datum().(common.Symbol); ok {
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

type importSetResolution struct {
	identifierSpec identifierSpec
}

type identifierSpec interface {
	resolve(bindings common.BindingSet) (common.BindingSet, error)
}

type identifierSpecAll struct{}

func (spec identifierSpecAll) resolve(bindings common.BindingSet) (common.BindingSet, error) {
	return bindings, nil
}

type identifierSpecOnly struct {
	identifierSpec identifierSpec
	identifiers    []common.Symbol
}

func (spec identifierSpecOnly) resolve(bindings common.BindingSet) (common.BindingSet, error) {
	var err error
	bindings, err = spec.identifierSpec.resolve(bindings)
	if err != nil {
		return nil, err
	}
	for _, id := range spec.identifiers {
		found := false
		for _, locations := range bindings {
			if _, ok := locations[id]; ok {
				found = true
			}
		}
		if !found {
			return nil, fmt.Errorf("only: unexported identifier %v", id)
		}
	}
	filteredBindings := make(common.BindingSet)
	for level, locations := range bindings {
		filteredBindings[level] = make(map[common.Symbol]common.Location)
		for id, location := range locations {
			found := false
			for _, included := range spec.identifiers {
				if id == included {
					found = true
				}
			}
			if found {
				filteredBindings[level][id] = location
			}
		}
	}
	return filteredBindings, nil
}

type identifierSpecExcept struct {
	identifierSpec identifierSpec
	identifiers    []common.Symbol
}

func (spec identifierSpecExcept) resolve(bindings common.BindingSet) (common.BindingSet, error) {
	var err error
	bindings, err = spec.identifierSpec.resolve(bindings)
	if err != nil {
		return nil, err
	}
	for _, id := range spec.identifiers {
		found := false
		for _, locations := range bindings {
			if _, ok := locations[id]; ok {
				found = true
			}
		}
		if !found {
			return nil, fmt.Errorf("except: unexported identifier %v", id)
		}
	}
	filteredBindings := make(common.BindingSet)
	for level, locations := range bindings {
		filteredBindings[level] = make(map[common.Symbol]common.Location)
		for id, location := range locations {
			found := false
			for _, excluded := range spec.identifiers {
				if id == excluded {
					found = true
				}
			}
			if !found {
				filteredBindings[level][id] = location
			}
		}
	}
	return filteredBindings, nil
}

type identifierSpecPrefix struct {
	identifierSpec identifierSpec
	identifier     common.Symbol
}

func (spec identifierSpecPrefix) resolve(bindings common.BindingSet) (common.BindingSet, error) {
	var err error
	bindings, err = spec.identifierSpec.resolve(bindings)
	if err != nil {
		return nil, err
	}
	prefixedBindings := make(common.BindingSet)
	for level, locations := range bindings {
		prefixedBindings[level] = make(map[common.Symbol]common.Location)
		for id, location := range locations {
			prefixedBindings[level][common.Symbol(spec.identifier+id)] = location
		}
	}
	return prefixedBindings, nil
}

type identifierSpecRename struct {
	identifierSpec     identifierSpec
	identifierBindings []identifierBinding
}

func (spec identifierSpecRename) resolve(bindings common.BindingSet) (common.BindingSet, error) {
	var err error
	bindings, err = spec.identifierSpec.resolve(bindings)
	if err != nil {
		return nil, err
	}
	for _, identifierBinding := range spec.identifierBindings {
		found := false
		for _, locations := range bindings {
			if _, ok := locations[identifierBinding.external]; ok {
				found = true
			}
		}
		if !found {
			return nil, fmt.Errorf("rename: unexported identifier %v", identifierBinding.external)
		}
	}
	renamedBindings := make(common.BindingSet)
	for level, locations := range bindings {
		renamedBindings[level] = make(map[common.Symbol]common.Location)
		for id, location := range locations {
			for _, identifierBinding := range spec.identifierBindings {
				if identifierBinding.external == id {
					id = identifierBinding.internal
				}
			}
			renamedBindings[level][id] = location
		}
	}
	return renamedBindings, nil
}

type importSetOnly struct {
	importSet   importSet
	identifiers []common.Symbol
}

func (set importSetOnly) resolve(library *Library) (importSetResolution, bool) {
	subRes, ok := set.importSet.resolve(library)
	return importSetResolution{
		identifierSpecOnly{
			subRes.identifierSpec,
			set.identifiers,
		},
	}, ok
}

func (set importSetOnly) libraryName() []common.Symbol {
	return set.importSet.libraryName()
}

type importSetExcept struct {
	importSet   importSet
	identifiers []common.Symbol
}

func (set importSetExcept) resolve(library *Library) (importSetResolution, bool) {
	subRes, ok := set.importSet.resolve(library)
	return importSetResolution{
		identifierSpecExcept{
			subRes.identifierSpec,
			set.identifiers,
		},
	}, ok
}

func (set importSetExcept) libraryName() []common.Symbol {
	return set.importSet.libraryName()
}

type importSetPrefix struct {
	importSet  importSet
	identifier common.Symbol
}

func (set importSetPrefix) resolve(library *Library) (importSetResolution, bool) {
	subRes, ok := set.importSet.resolve(library)
	return importSetResolution{
		identifierSpecPrefix{
			subRes.identifierSpec,
			set.identifier,
		},
	}, ok
}

func (set importSetPrefix) libraryName() []common.Symbol {
	return set.importSet.libraryName()
}

type importSetRename struct {
	importSet          importSet
	identifierBindings []identifierBinding
}

func (set importSetRename) resolve(library *Library) (importSetResolution, bool) {
	subRes, ok := set.importSet.resolve(library)
	return importSetResolution{
		identifierSpecRename{
			subRes.identifierSpec,
			set.identifierBindings,
		},
	}, ok
}

func (set importSetRename) libraryName() []common.Symbol {
	return set.importSet.libraryName()
}

type importSetLibraryReference struct {
	name             []common.Symbol
	versionReference versionReference
}

func newLibraryReference(d common.WrappedSyntax) (importSetLibraryReference, error) {
	var ref importSetLibraryReference
	result, ok, err := util.MatchSyntax(d, PatternLibraryReference, map[common.Symbol]common.Location{})
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
		if name, ok := libraryName[i].(common.WrappedSyntax).Datum().(common.Symbol); ok {
			ref.name = append(ref.name, name)
		} else {
			break
		}
	}
	ref.versionReference = versionReferenceSubVersionReferences{}
	if i == len(libraryName)-1 {
		vRef, err := newVersionReference(libraryName[i].(common.WrappedSyntax))
		if err != nil {
			return importSetLibraryReference{}, err
		}
		ref.versionReference = vRef
	} else if i < len(libraryName)-1 {
		return importSetLibraryReference{}, fmt.Errorf("runtime: malformed library reference")
	}
	return ref, nil
}

func (set importSetLibraryReference) resolve(library *Library) (importSetResolution, bool) {
	ok := sameName(set.name, library.name) && set.versionReference.resolve(library)
	return importSetResolution{identifierSpecAll{}}, ok
}

func (set importSetLibraryReference) libraryName() []common.Symbol {
	return set.name
}

type versionReference interface {
	resolve(*Library) bool
}

func newVersionReference(d common.WrappedSyntax) (versionReference, error) {
	if result, ok, err := util.MatchSyntax(d, PatternVersionReferenceAnd, map[common.Symbol]common.Location{
		common.Symbol("and"): nil,
	}); err != nil {
		return nil, err
	} else if ok {
		var vRefs []versionReference
		for _, d := range result[common.Symbol("version-reference")].([]interface{}) {
			vRef, err := newVersionReference(d.(common.WrappedSyntax))
			if err != nil {
				return nil, err
			}
			vRefs = append(vRefs, vRef)
		}
		return versionReferenceAnd{vRefs}, nil
	}
	if result, ok, err := util.MatchSyntax(d, PatternVersionReferenceOr, map[common.Symbol]common.Location{
		common.Symbol("or"): nil,
	}); err != nil {
		return nil, err
	} else if ok {
		var vRefs []versionReference
		for _, d := range result[common.Symbol("version-reference")].([]interface{}) {
			vRef, err := newVersionReference(d.(common.WrappedSyntax))
			if err != nil {
				return nil, err
			}
			vRefs = append(vRefs, vRef)
		}
		return versionReferenceOr{vRefs}, nil
	}
	if result, ok, err := util.MatchSyntax(d, PatternVersionReferenceNot, map[common.Symbol]common.Location{
		common.Symbol("not"): nil,
	}); err != nil {
		return nil, err
	} else if ok {
		vRef, err := newVersionReference(result[common.Symbol("version-reference")].(common.WrappedSyntax))
		if err != nil {
			return nil, err
		}
		return versionReferenceNot{vRef}, nil
	}
	if result, ok, err := util.MatchSyntax(d, PatternSubVersionReferences, map[common.Symbol]common.Location{}); err != nil {
		return nil, err
	} else if ok {
		var subVRefs []subVersionReference
		for _, d := range result[common.Symbol("sub-version-reference")].([]interface{}) {
			subVRef, err := newSubVersionReference(d.(common.WrappedSyntax))
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

func (ref versionReferenceAnd) resolve(library *Library) bool {
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

func (ref versionReferenceOr) resolve(library *Library) bool {
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

func (ref versionReferenceNot) resolve(library *Library) bool {
	return !ref.versionReference.resolve(library)
}

type versionReferenceSubVersionReferences struct {
	subVersionReferences []subVersionReference
}

func (ref versionReferenceSubVersionReferences) resolve(library *Library) bool {
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

func newSubVersionReference(d common.WrappedSyntax) (subVersionReference, error) {
	if result, ok, err := util.MatchSyntax(d, PatternSubVersionReferenceGte, map[common.Symbol]common.Location{
		common.Symbol(">="): nil,
	}); err != nil {
		return nil, err
	} else if ok {
		subV, err := newSubVersion(result[common.Symbol("sub-version")].(common.WrappedSyntax))
		if err != nil {
			return nil, err
		}
		return subVersionReferenceGte{subV}, nil
	}
	if result, ok, err := util.MatchSyntax(d, PatternSubVersionReferenceLte, map[common.Symbol]common.Location{
		common.Symbol("<="): nil,
	}); err != nil {
		return nil, err
	} else if ok {
		subV, err := newSubVersion(result[common.Symbol("sub-version")].(common.WrappedSyntax))
		if err != nil {
			return nil, err
		}
		return subVersionReferenceLte{subV}, nil
	}
	if result, ok, err := util.MatchSyntax(d, PatternSubVersionReferenceAnd, map[common.Symbol]common.Location{
		common.Symbol("and"): nil,
	}); err != nil {
		return nil, err
	} else if ok {
		var subVRefs []subVersionReference
		for _, d := range result[common.Symbol("sub-version-reference")].([]interface{}) {
			subVRef, err := newSubVersionReference(d.(common.WrappedSyntax))
			if err != nil {
				return nil, err
			}
			subVRefs = append(subVRefs, subVRef)
		}
		return subVersionReferenceAnd{subVRefs}, nil
	}
	if result, ok, err := util.MatchSyntax(d, PatternSubVersionReferenceOr, map[common.Symbol]common.Location{
		common.Symbol("or"): nil,
	}); err != nil {
		return nil, err
	} else if ok {
		var subVRefs []subVersionReference
		for _, d := range result[common.Symbol("sub-version-reference")].([]interface{}) {
			subVRef, err := newSubVersionReference(d.(common.WrappedSyntax))
			if err != nil {
				return nil, err
			}
			subVRefs = append(subVRefs, subVRef)
		}
		return subVersionReferenceOr{subVRefs}, nil
	}
	if result, ok, err := util.MatchSyntax(d, PatternSubVersionReferenceNot, map[common.Symbol]common.Location{
		common.Symbol("not"): nil,
	}); err != nil {
		return nil, err
	} else if ok {
		subVRef, err := newSubVersionReference(result[common.Symbol("sub-version-reference")].(common.WrappedSyntax))
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

func newSubVersion(d common.WrappedSyntax) (subVersion, error) {
	number, ok := d.Datum().(common.Number)
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
