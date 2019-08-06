package r6rs

import (
	"fmt"

	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/read"
)

type CoreTransformer struct {
	coreForm func(ExpansionContext, common.Syntax) (CoreForm, error)
}

func NewCoreTransformer(coreForm func(ExpansionContext, common.Syntax) (CoreForm, error)) CoreTransformer {
	return CoreTransformer{coreForm}
}

func (CoreTransformer) Call(c common.Continuation, args ...common.Datum) (common.Evaluation, error) {
	return common.ErrorC(fmt.Errorf("core transformer called"))
}

var patternMacroUseList = common.MustCompileSimplePattern(read.MustReadDatum("(keyword _ ...)"))

type CoreExpander struct {
	inner Expander
}

func NewSelfReferentialCoreExpander() *CoreExpander {
	expander := &CoreExpander{}
	expander.inner = expander
	return expander
}

func NewCoreExpander(inner Expander) *CoreExpander {
	return &CoreExpander{inner}
}

func (e CoreExpander) Expand(syntax common.Syntax, env common.Environment) (CoreForm, error) {
	var err error
	for {
		transformer := e.MatchTransformer(syntax, env, patternMacroUseList)
		if transformer == nil {
			return nil, fmt.Errorf("unhandled syntax %v at %v", common.Write(syntax.Datum()), syntax.SourceLocation())
		}
		if coreForm, ok, err := e.HandleCoreTransformer(transformer, syntax, env); err != nil {
			return nil, err
		} else if ok {
			return coreForm, nil
		}
		syntax, err = e.ApplyTransformer(transformer, syntax)
		if err != nil {
			return nil, err
		}
	}
}

func (e CoreExpander) MatchTransformer(syntax common.Syntax, env common.Environment, pattern common.SimplePattern) common.Procedure {
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
	syntacticAbstraction, ok := role.(common.SyntacticAbstraction)
	if !ok {
		return nil
	}
	return syntacticAbstraction.Transformer
}

func (e CoreExpander) HandleCoreTransformer(transformer common.Procedure, syntax common.Syntax, env common.Environment) (CoreForm, bool, error) {
	if transformer, ok := transformer.(CoreTransformer); ok {
		ctx := ExpansionContext{Expander: e.inner, Env: env}
		coreForm, err := transformer.coreForm(ctx, syntax)
		return coreForm, true, err
	}
	return nil, false, nil
}

func (e CoreExpander) ApplyTransformer(transformer common.Procedure, syntax common.Syntax) (common.Syntax, error) {
	mark := common.NewMark()
	input := syntax.Mark(mark)
	output, err := common.WithEscape(func(escape common.Continuation) (common.Evaluation, error) {
		return transformer.Call(escape, input.Datum())
	})
	if err != nil {
		return common.Syntax{}, err
	}
	return common.NewSyntax(output).Mark(mark), nil
}
