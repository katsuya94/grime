package runtime

import (
	"fmt"
	"strings"

	"github.com/katsuya94/grime/common"
)

type identifierTransformer interface {
	transform(common.Symbol) (common.Symbol, bool)
	error() error
}

type identifierSpec interface {
	transformer() identifierTransformer
}

type identifierSpecAll struct{}

func (spec identifierSpecAll) transformer() identifierTransformer {
	return identifierTransformerAll{}
}

type identifierTransformerAll struct{}

func (spec identifierTransformerAll) transform(name common.Symbol) (common.Symbol, bool) {
	return name, true
}

func (spec identifierTransformerAll) error() error {
	return nil
}

type identifierSpecOnly struct {
	identifierSpec identifierSpec
	identifiers    []common.Symbol
}

func (spec identifierSpecOnly) transformer() identifierTransformer {
	unreferenced := make(map[common.Symbol]struct{}, len(spec.identifiers))
	for _, id := range spec.identifiers {
		unreferenced[id] = struct{}{}
	}
	return identifierTransformerOnly{spec, spec.identifierSpec.transformer(), unreferenced}
}

type identifierTransformerOnly struct {
	identifierSpecOnly
	inner        identifierTransformer
	unreferenced map[common.Symbol]struct{}
}

func (spec identifierTransformerOnly) transform(name common.Symbol) (common.Symbol, bool) {
	name, ok := spec.inner.transform(name)
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

func (spec identifierTransformerOnly) error() error {
	err := spec.inner.error()
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

func (spec identifierSpecExcept) transformer() identifierTransformer {
	unreferenced := make(map[common.Symbol]struct{}, len(spec.identifiers))
	for _, id := range spec.identifiers {
		unreferenced[id] = struct{}{}
	}
	return identifierTransformerExcept{spec, spec.identifierSpec.transformer(), unreferenced}
}

type identifierTransformerExcept struct {
	identifierSpecExcept
	inner        identifierTransformer
	unreferenced map[common.Symbol]struct{}
}

func (spec identifierTransformerExcept) transform(name common.Symbol) (common.Symbol, bool) {
	name, ok := spec.inner.transform(name)
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

func (spec identifierTransformerExcept) error() error {
	err := spec.inner.error()
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

func (spec identifierSpecPrefix) transformer() identifierTransformer {
	return identifierTransformerPrefix{spec, spec.identifierSpec.transformer()}
}

type identifierTransformerPrefix struct {
	identifierSpecPrefix
	inner identifierTransformer
}

func (spec identifierTransformerPrefix) transform(name common.Symbol) (common.Symbol, bool) {
	name, ok := spec.inner.transform(name)
	if !ok {
		return common.Symbol(""), false
	}
	return common.Symbol(spec.identifier + name), true
}

func (spec identifierTransformerPrefix) error() error {
	return spec.inner.error()
}

type identifierSpecRename struct {
	identifierSpec     identifierSpec
	identifierBindings []identifierBinding
}

func (spec identifierSpecRename) transformer() identifierTransformer {
	unreferenced := make(map[common.Symbol]struct{}, len(spec.identifierBindings))
	for _, identifierBinding := range spec.identifierBindings {
		unreferenced[identifierBinding.external] = struct{}{}
	}
	return identifierTransformerRename{spec, spec.identifierSpec.transformer(), unreferenced}
}

type identifierTransformerRename struct {
	identifierSpecRename
	inner        identifierTransformer
	unreferenced map[common.Symbol]struct{}
}

func (spec identifierTransformerRename) transform(name common.Symbol) (common.Symbol, bool) {
	name, ok := spec.inner.transform(name)
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

func (spec identifierTransformerRename) error() error {
	err := spec.inner.error()
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
