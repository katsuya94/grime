package runtime

import (
	"fmt"
	"strconv"

	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/read"
)

var (
	PatternImportSpecFor          = common.Pattern(read.MustReadDatum("(for import-set import-level ...)"))
	PatternImportLevelMeta        = common.Pattern(read.MustReadDatum("(meta n)"))
	PatternImportLevelRun         = common.Pattern(read.MustReadDatum("run"))
	PatternImportLevelExpand      = common.Pattern(read.MustReadDatum("expand"))
	PatternLibraryReference       = common.Pattern(read.MustReadDatum("(library-name ...)"))
	PatternImportSetLibrary       = common.Pattern(read.MustReadDatum("(library library-reference)"))
	PatternImportSetOnly          = common.Pattern(read.MustReadDatum("(only import-set identifier ...)"))
	PatternImportSetExcept        = common.Pattern(read.MustReadDatum("(except import-set identifier ...)"))
	PatternImportSetPrefix        = common.Pattern(read.MustReadDatum("(prefix import-set identifier)"))
	PatternImportSetRename        = common.Pattern(read.MustReadDatum("(rename import-set (external internal) ...)"))
	PatternSubVersionReferences   = common.Pattern(read.MustReadDatum("(sub-version-reference ...)"))
	PatternVersionReferenceAnd    = common.Pattern(read.MustReadDatum("(and version-reference ...)"))
	PatternVersionReferenceOr     = common.Pattern(read.MustReadDatum("(or version-reference ...)"))
	PatternVersionReferenceNot    = common.Pattern(read.MustReadDatum("(not version-reference)"))
	PatternSubVersionReferenceGte = common.Pattern(read.MustReadDatum("(>= sub-version)"))
	PatternSubVersionReferenceLte = common.Pattern(read.MustReadDatum("(<= sub-version)"))
	PatternSubVersionReferenceAnd = common.Pattern(read.MustReadDatum("(and sub-version-reference ...)"))
	PatternSubVersionReferenceOr  = common.Pattern(read.MustReadDatum("(or sub-version-reference ...)"))
	PatternSubVersionReferenceNot = common.Pattern(read.MustReadDatum("(not sub-version-reference)"))
)

func id(s string) common.Identifier {
	return common.NewIdentifier(common.Symbol(s))
}

func ids(s ...string) []common.Identifier {
	ids := make([]common.Identifier, len(s))
	for i, s := range s {
		ids[i] = id(s)
	}
	return ids
}

func extract(result common.MatchResultSet, s string) interface{} {
	return result.Get(id(s))
}

type importSpec struct {
	importSet importSet
	levels    []int
}

func newImportSpec(d common.Syntax) (importSpec, error) {
	if result, ok, err := common.MatchSyntaxSimple(d, PatternImportSpecFor, "for"); err != nil {
		return importSpec{}, err
	} else if ok {
		iSet, err := newImportSet(result.Get("import-set").(common.Syntax))
		if err != nil {
			return importSpec{}, err
		}
		var levels []int
		for _, d := range result.Get("import-level").([]interface{}) {
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

func newImportLevel(d common.Syntax) (int, error) {
	if result, ok, err := common.MatchSyntaxSimple(d, PatternImportLevelMeta, "meta"); err != nil {
		return 0, err
	} else if ok {
		number, ok := result.Get("n").(common.Syntax).Unwrap().(common.Number)
		if !ok {
			return 0, fmt.Errorf("runtime: malformed import level")
		}
		n, err := strconv.ParseInt(string(number), 10, 0)
		if err != nil {
			return 0, err
		}
		return int(n), nil
	}
	if _, ok, err := common.MatchSyntaxSimple(d, PatternImportLevelRun, "run"); err != nil {
		return 0, err
	} else if ok {
		return 0, nil
	}
	if _, ok, err := common.MatchSyntaxSimple(d, PatternImportLevelExpand, "expand"); err != nil {
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

func newImportSet(d common.Syntax) (importSet, error) {
	if result, ok, err := common.MatchSyntaxSimple(d, PatternImportSetLibrary, "library"); err != nil {
		return nil, err
	} else if ok {
		return newLibraryReference(result.Get("library-reference").(common.Syntax))
	}
	if result, ok, err := common.MatchSyntaxSimple(d, PatternImportSetOnly, "only"); err != nil {
		return nil, err
	} else if ok {
		iSet, err := newImportSet(result.Get("import-set").(common.Syntax))
		if err != nil {
			return nil, err
		}
		var ids []common.Symbol
		for _, d := range result.Get("identifier").([]interface{}) {
			id, ok := d.(common.Syntax).Unwrap().(common.Symbol)
			if !ok {
				return nil, fmt.Errorf("runtime: malformed import set")
			}
			ids = append(ids, id)
		}
		return importSetOnly{iSet, ids}, nil
	}
	if result, ok, err := common.MatchSyntaxSimple(d, PatternImportSetExcept, "except"); err != nil {
		return nil, err
	} else if ok {
		iSet, err := newImportSet(result.Get("import-set").(common.Syntax))
		if err != nil {
			return nil, err
		}
		var ids []common.Symbol
		for _, d := range result.Get("identifier").([]interface{}) {
			id, ok := d.(common.Syntax).Unwrap().(common.Symbol)
			if !ok {
				return nil, fmt.Errorf("runtime: malformed import set")
			}
			ids = append(ids, id)
		}
		return importSetExcept{iSet, ids}, nil
	}
	if result, ok, err := common.MatchSyntaxSimple(d, PatternImportSetPrefix, "prefix"); err != nil {
		return nil, err
	} else if ok {
		iSet, err := newImportSet(result.Get("import-set").(common.Syntax))
		if err != nil {
			return nil, err
		}
		id, ok := result.Get("identifier").(common.Syntax).Unwrap().(common.Symbol)
		if !ok {
			return nil, fmt.Errorf("runtime: malformed import set")
		}
		return importSetPrefix{iSet, id}, nil
	}
	if result, ok, err := common.MatchSyntaxSimple(d, PatternImportSetRename, "rename"); err != nil {
		return nil, err
	} else if ok {
		iSet, err := newImportSet(result.Get("import-set").(common.Syntax))
		if err != nil {
			return nil, err
		}
		var externalIdentifiers []common.Symbol
		for _, d := range result.Get("external").([]interface{}) {
			if id, ok := d.(common.Syntax).Unwrap().(common.Symbol); ok {
				externalIdentifiers = append(externalIdentifiers, id)
			} else {
				return nil, fmt.Errorf("runtime: malformed import set")
			}
		}
		var internalIdentifiers []common.Symbol
		for _, d := range result.Get("internal").([]interface{}) {
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
	filteredBindings := common.NewBindingSet()
	for level, locations := range bindings {
		for id, location := range locations {
			found := false
			for _, included := range spec.identifiers {
				if id == included {
					found = true
				}
			}
			if found {
				filteredBindings.Set(id, level, location)
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
	filteredBindings := common.NewBindingSet()
	for level, locations := range bindings {
		for id, location := range locations {
			found := false
			for _, excluded := range spec.identifiers {
				if id == excluded {
					found = true
				}
			}
			if !found {
				filteredBindings.Set(id, level, location)
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
	prefixedBindings := common.NewBindingSet()
	for level, locations := range bindings {
		for id, location := range locations {
			prefixedBindings.Set(common.Symbol(spec.identifier+id), level, location)
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
	renamedBindings := common.NewBindingSet()
	for level, locations := range bindings {
		for id, location := range locations {
			for _, identifierBinding := range spec.identifierBindings {
				if identifierBinding.external == id {
					id = identifierBinding.internal
				}
			}
			renamedBindings.Set(id, level, location)
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

func newLibraryReference(d common.Syntax) (importSetLibraryReference, error) {
	var ref importSetLibraryReference
	result, ok, err := common.MatchSyntaxSimple(d, PatternLibraryReference)
	if err != nil {
		return importSetLibraryReference{}, err
	} else if !ok {
		return importSetLibraryReference{}, fmt.Errorf("runtime: malformed library reference")
	}
	libraryName := result.Get("library-name").([]interface{})
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

func newVersionReference(d common.Syntax) (versionReference, error) {
	if result, ok, err := common.MatchSyntaxSimple(d, PatternVersionReferenceAnd, "and"); err != nil {
		return nil, err
	} else if ok {
		var vRefs []versionReference
		for _, d := range result.Get("version-reference").([]interface{}) {
			vRef, err := newVersionReference(d.(common.Syntax))
			if err != nil {
				return nil, err
			}
			vRefs = append(vRefs, vRef)
		}
		return versionReferenceAnd{vRefs}, nil
	}
	if result, ok, err := common.MatchSyntaxSimple(d, PatternVersionReferenceOr, "or"); err != nil {
		return nil, err
	} else if ok {
		var vRefs []versionReference
		for _, d := range result.Get("version-reference").([]interface{}) {
			vRef, err := newVersionReference(d.(common.Syntax))
			if err != nil {
				return nil, err
			}
			vRefs = append(vRefs, vRef)
		}
		return versionReferenceOr{vRefs}, nil
	}
	if result, ok, err := common.MatchSyntaxSimple(d, PatternVersionReferenceNot, "not"); err != nil {
		return nil, err
	} else if ok {
		vRef, err := newVersionReference(result.Get("version-reference").(common.Syntax))
		if err != nil {
			return nil, err
		}
		return versionReferenceNot{vRef}, nil
	}
	if result, ok, err := common.MatchSyntaxSimple(d, PatternSubVersionReferences); err != nil {
		return nil, err
	} else if ok {
		var subVRefs []subVersionReference
		for _, d := range result.Get("sub-version-reference").([]interface{}) {
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

func newSubVersionReference(d common.Syntax) (subVersionReference, error) {
	if result, ok, err := common.MatchSyntaxSimple(d, PatternSubVersionReferenceGte, ">="); err != nil {
		return nil, err
	} else if ok {
		subV, err := newSubVersion(result.Get("sub-version").(common.Syntax))
		if err != nil {
			return nil, err
		}
		return subVersionReferenceGte{subV}, nil
	}
	if result, ok, err := common.MatchSyntaxSimple(d, PatternSubVersionReferenceLte, "<="); err != nil {
		return nil, err
	} else if ok {
		subV, err := newSubVersion(result.Get("sub-version").(common.Syntax))
		if err != nil {
			return nil, err
		}
		return subVersionReferenceLte{subV}, nil
	}
	if result, ok, err := common.MatchSyntaxSimple(d, PatternSubVersionReferenceAnd, "and"); err != nil {
		return nil, err
	} else if ok {
		var subVRefs []subVersionReference
		for _, d := range result.Get("sub-version-reference").([]interface{}) {
			subVRef, err := newSubVersionReference(d.(common.Syntax))
			if err != nil {
				return nil, err
			}
			subVRefs = append(subVRefs, subVRef)
		}
		return subVersionReferenceAnd{subVRefs}, nil
	}
	if result, ok, err := common.MatchSyntaxSimple(d, PatternSubVersionReferenceOr, "or"); err != nil {
		return nil, err
	} else if ok {
		var subVRefs []subVersionReference
		for _, d := range result.Get("sub-version-reference").([]interface{}) {
			subVRef, err := newSubVersionReference(d.(common.Syntax))
			if err != nil {
				return nil, err
			}
			subVRefs = append(subVRefs, subVRef)
		}
		return subVersionReferenceOr{subVRefs}, nil
	}
	if result, ok, err := common.MatchSyntaxSimple(d, PatternSubVersionReferenceNot, "not"); err != nil {
		return nil, err
	} else if ok {
		subVRef, err := newSubVersionReference(result.Get("sub-version-reference").(common.Syntax))
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
