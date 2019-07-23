package r6rs

import (
	"fmt"

	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/read"
)

type coreTransformer struct {
	coreForm func(ExpansionContext, common.Syntax) (CoreForm, error)
}

func (coreTransformer) Call(c common.Continuation, args ...common.Datum) (common.Evaluation, error) {
	return common.ErrorC(fmt.Errorf("core transformer called"))
}

type Expander interface {
	Expand(ExpansionContext, common.Syntax) (CoreForm, error)
	LookupBinding(common.Identifier) (Binding, bool)
}

type EnvEntry interface{}

type VariableEnvEntry struct{}

type KeywordEnvEntry struct {
	transformer common.Procedure
}

type ExpansionContext struct {
	Expander Expander
	env      map[Binding]EnvEntry
}

func (ctx ExpansionContext) Lookup(b Binding) EnvEntry {
	return ctx.env[b]
}

var (
	patternMacroUseList = common.MustCompileSimplePattern(read.MustReadDatum("(keyword _ ...)"))
)

type CoreExpander struct {
	bindingsTable BindingsTable
}

func NewCoreExpander(bindingsTable BindingsTable) CoreExpander {
	return CoreExpander{bindingsTable}
}

func (e CoreExpander) Expand(ctx ExpansionContext, syntax common.Syntax) (CoreForm, error) {
	// TODO: what happens if we limit this to WrappedSyntax only?
	// r6rs-lib 12.3 seems to imply that transformers should only take wrapped syntax objects,
	for {
		switch transformer := syntacticAbstraction(ctx, syntax).(type) {
		case coreTransformer:
			return transformer.coreForm(ctx, syntax)
		// partialTransformer
		default:
			panic("unhandled transformer")
		}
	}
}

func (e CoreExpander) LookupBinding(id common.Identifier) (Binding, bool) {
	return e.bindingsTable.LookupBinding(id)
}

func syntacticAbstraction(ctx ExpansionContext, syntax common.Syntax) common.Procedure {
	var transformer common.Procedure
	transformer = transformerMatching(ctx, syntax, patternMacroUseList)
	if transformer != nil {
		return transformer
	}
	return nil
}

func transformerMatching(ctx ExpansionContext, syntax common.Syntax, pattern common.SimplePattern) common.Procedure {
	result, ok := pattern.Match(syntax)
	if !ok {
		return nil
	}
	id, ok := result[common.Symbol("keyword")].(common.Syntax).Identifier()
	if !ok {
		return nil
	}
	binding, ok := ctx.Expander.LookupBinding(id)
	if !ok {
		return nil
	}
	entry := ctx.Lookup(binding)
	return nil
}
