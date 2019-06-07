package runtime

import (
	"fmt"
	"strings"

	"github.com/katsuya94/grime/common"
)

type identifierSpec interface {
	common.IdentifierTransformerFactory
}

type identifierSpecAll struct{}

func (spec identifierSpecAll) New() common.IdentifierTransformer {
	return identifierTransformerAll{}
}

type identifierTransformerAll struct{}

func (spec identifierTransformerAll) Transform(name common.Symbol) (common.Symbol, bool) {
	return name, true
}

func (spec identifierTransformerAll) Error() error {
	return nil
}

type identifierSpecOnly struct {
	identifierSpec identifierSpec
	identifiers    []common.Symbol
}

func (spec identifierSpecOnly) New() common.IdentifierTransformer {
	unreferenced := make(map[common.Symbol]struct{}, len(spec.identifiers))
	for _, id := range spec.identifiers {
		unreferenced[id] = struct{}{}
	}
	return identifierTransformerOnly{spec, spec.identifierSpec.New(), unreferenced}
}

type identifierTransformerOnly struct {
	identifierSpecOnly
	inner        common.IdentifierTransformer
	unreferenced map[common.Symbol]struct{}
}

func (spec identifierTransformerOnly) Transform(name common.Symbol) (common.Symbol, bool) {
	name, ok := spec.inner.Transform(name)
	if !ok {
		return common.Symbol(""), false
	}
	for _, id := range spec.identifiers {
		if id == name {
			delete(spec.unreferenced, name)
			return name, true
		}
	}
	return common.Symbol(""), false
}

func (spec identifierTransformerOnly) Error() error {
	err := spec.inner.Error()
	if err != nil {
		return err
	}
	if len(spec.unreferenced) != 0 {
		return fmt.Errorf("only: unexported identifier(s) %v", joinSymbolSet(spec.unreferenced))
	}
	return nil
}

type identifierSpecExcept struct {
	identifierSpec identifierSpec
	identifiers    []common.Symbol
}

func (spec identifierSpecExcept) New() common.IdentifierTransformer {
	unreferenced := make(map[common.Symbol]struct{}, len(spec.identifiers))
	for _, id := range spec.identifiers {
		unreferenced[id] = struct{}{}
	}
	return identifierTransformerExcept{spec, spec.identifierSpec.New(), unreferenced}
}

type identifierTransformerExcept struct {
	identifierSpecExcept
	inner        common.IdentifierTransformer
	unreferenced map[common.Symbol]struct{}
}

func (spec identifierTransformerExcept) Transform(name common.Symbol) (common.Symbol, bool) {
	name, ok := spec.inner.Transform(name)
	if !ok {
		return common.Symbol(""), false
	}
	for _, id := range spec.identifiers {
		if id == name {
			delete(spec.unreferenced, name)
			return common.Symbol(""), false
		}
	}
	return name, true
}

func (spec identifierTransformerExcept) Error() error {
	err := spec.inner.Error()
	if err != nil {
		return err
	}
	if len(spec.unreferenced) != 0 {
		return fmt.Errorf("only: unexported identifier(s) %v", joinSymbolSet(spec.unreferenced))
	}
	return nil
}

type identifierSpecPrefix struct {
	identifierSpec identifierSpec
	identifier     common.Symbol
}

func (spec identifierSpecPrefix) New() common.IdentifierTransformer {
	return identifierTransformerPrefix{spec, spec.identifierSpec.New()}
}

type identifierTransformerPrefix struct {
	identifierSpecPrefix
	inner common.IdentifierTransformer
}

func (spec identifierTransformerPrefix) Transform(name common.Symbol) (common.Symbol, bool) {
	name, ok := spec.inner.Transform(name)
	if !ok {
		return common.Symbol(""), false
	}
	return common.Symbol(spec.identifier + name), true
}

func (spec identifierTransformerPrefix) Error() error {
	return spec.inner.Error()
}

type identifierSpecRename struct {
	identifierSpec     identifierSpec
	identifierBindings []identifierBinding
}

func (spec identifierSpecRename) New() common.IdentifierTransformer {
	unreferenced := make(map[common.Symbol]struct{}, len(spec.identifierBindings))
	for _, identifierBinding := range spec.identifierBindings {
		unreferenced[identifierBinding.external] = struct{}{}
	}
	return identifierTransformerRename{spec, spec.identifierSpec.New(), unreferenced}
}

type identifierTransformerRename struct {
	identifierSpecRename
	inner        common.IdentifierTransformer
	unreferenced map[common.Symbol]struct{}
}

func (spec identifierTransformerRename) Transform(name common.Symbol) (common.Symbol, bool) {
	name, ok := spec.inner.Transform(name)
	if !ok {
		return common.Symbol(""), false
	}
	for _, identifierBinding := range spec.identifierBindings {
		if identifierBinding.external == name {
			delete(spec.unreferenced, name)
			return identifierBinding.internal, true
		}
	}
	return name, true
}

func (spec identifierTransformerRename) Error() error {
	err := spec.inner.Error()
	if err != nil {
		return err
	}
	if len(spec.unreferenced) != 0 {
		return fmt.Errorf("rename: unexported identifier(s) %v", joinSymbolSet(spec.unreferenced))
	}
	return nil
}

func joinSymbolSet(symbols map[common.Symbol]struct{}) string {
	strs := make([]string, 0, len(symbols))
	for symbol := range symbols {
		strs = append(strs, string(symbol))
	}
	return strings.Join(strs, ", ")
}
