package r6rs

import (
	"fmt"

	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/read"
)

type ExpansionContext struct {
	Expander Expander
	Env      Environment
}

func (ctx ExpansionContext) Expand(syntax common.Syntax) (CoreForm, error) {
	return ctx.Expander.Expand(syntax, ctx.Env)
}

type coreTransformer struct {
	coreForm func(ExpansionContext, common.Syntax) (CoreForm, error)
}

func (coreTransformer) Call(c common.Continuation, args ...common.Datum) (common.Evaluation, error) {
	return common.ErrorC(fmt.Errorf("core transformer called"))
}

type Expander interface {
	Expand(common.Syntax, Environment) (CoreForm, error)
}

var (
	patternMacroUseList = common.MustCompileSimplePattern(read.MustReadDatum("(keyword _ ...)"))
)

type CoreExpander struct{}

func NewCoreExpander() CoreExpander {
	return CoreExpander{}
}

func (e CoreExpander) Expand(syntax common.Syntax, env Environment) (CoreForm, error) {
	for {
		switch transformer := e.syntacticAbstraction(syntax, env).(type) {
		case coreTransformer:
			ctx := ExpansionContext{Expander: e, Env: env}
			return transformer.coreForm(ctx, syntax)
		// partialTransformer
		default:
			panic("unhandled transformer")
		}
	}
}

func (e CoreExpander) syntacticAbstraction(syntax common.Syntax, env Environment) common.Procedure {
	var transformer common.Procedure
	transformer = e.transformerMatching(syntax, env, patternMacroUseList)
	if transformer != nil {
		return transformer
	}
	return nil
}

func (e CoreExpander) transformerMatching(syntax common.Syntax, env Environment, pattern common.SimplePattern) common.Procedure {
	result, ok := pattern.Match(syntax)
	if !ok {
		return nil
	}
	id, ok := result[common.Symbol("keyword")].(common.Syntax).Identifier()
	if !ok {
		return nil
	}
	binding, ok := id.Binding()
	if !ok {
		return nil
	}
	role := env.Lookup(binding)
	if role == nil {
		return nil
	}
	syntacticAbstraction, ok := role.(SyntacticAbstraction)
	if !ok {
		return nil
	}
	return syntacticAbstraction.transformer
}
