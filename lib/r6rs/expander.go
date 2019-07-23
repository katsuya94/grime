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
}

type ExpansionContext struct {
	Expander Expander
}

var (
	patternMacroUseList = common.MustCompileSimplePattern(read.MustReadDatum("(keyword _ ...)"))
)

type CoreExpander struct{}

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
	binding := id.Binding()
	if binding == nil {
		return nil
	}
	// ctx.Lookup(binding)
	return nil
}
