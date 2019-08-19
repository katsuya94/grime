package r6rs

import (
	"fmt"
	"reflect"

	"github.com/katsuya94/grime/common"
)

// TODO: support outputting annotated diff

func Equal(a common.Syntax, b common.Syntax) bool {
	ok, _ := equal(a, b)
	return ok
}

func equal(a common.Syntax, b common.Syntax) (bool, substitutionSet) {
	if aPair, aOk := a.Pair(); aOk {
		if bPair, bOk := b.Pair(); aOk && bOk {
			firstEqual, firstSubstitutions := equal(common.NewSyntax(aPair.First), common.NewSyntax(bPair.First))
			restEqual, restSubstitutions := equal(common.NewSyntax(aPair.Rest), common.NewSyntax(bPair.Rest))
			if !firstEqual || !restEqual {
				return false, nil
			}
			ok, substitutions := mergeSubstitutions(firstSubstitutions, restSubstitutions)
			if !ok {
				return false, nil
			}
			return true, substitutions
		} else if aOk != bOk {
			return false, nil
		}
	}
	if aId, aOk := a.Identifier(); aOk {
		if bId, bOk := b.Identifier(); aOk && bOk {
			aBinding := aId.Binding()
			bBinding := bId.Binding()
			aCore := false
			bCore := false
			for _, coreBinding := range coreBindings {
				if aBinding == coreBinding {
					aCore = true
				}
				if bBinding == coreBinding {
					bCore = true
				}
			}
			if aCore && bCore {
				return aBinding == bBinding, nil
			}
			if !aCore && !bCore {
				return true, substitutionSet{substitutionSetEntry{aId, bId}}
			}
			return false, nil
		} else if aOk != bOk {
			return false, nil
		}
	}
	aWrapped, ok := a.Datum().(common.WrappedSyntax)
	if !ok {
		panic(fmt.Sprintf("not syntax: %v", common.Write(a.Datum())))
	}
	bWrapped, ok := b.Datum().(common.WrappedSyntax)
	if !ok {
		panic(fmt.Sprintf("not syntax: %v", common.Write(b.Datum())))
	}
	return reflect.DeepEqual(aWrapped.Datum(), bWrapped.Datum()), nil
}

type substitutionSet []substitutionSetEntry

type substitutionSetEntry struct {
	a common.Identifier
	b common.Identifier
}

func mergeSubstitutions(l substitutionSet, r substitutionSet) (bool, substitutionSet) {
	n := len(l)
	if len(r) > n {
		n = len(r)
	}
	substitutions := make(substitutionSet, len(l), n)
	for i := range l {
		substitutions[i] = l[i]
	}
	for _, rEntry := range r {
		found := false
		for _, lEntry := range substitutions {
			aEqual := rEntry.a.BoundEqual(lEntry.a)
			bEqual := rEntry.b.BoundEqual(lEntry.b)
			if aEqual && bEqual {
				found = true
				break
			} else if aEqual != bEqual {
				return false, nil
			}
		}
		if !found {
			substitutions = append(substitutions, rEntry)
		}
	}
	return true, substitutions
}
