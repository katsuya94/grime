package r6rs

import (
	"fmt"

	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/read"
)

type CoreTransformer struct {
	transform func(ExpansionContext, common.Syntax, *common.M) (CoreForm, error)
}

func (CoreTransformer) Call(c common.Continuation, args ...common.Datum) (common.Evaluation, error) {
	return common.ErrorC(fmt.Errorf("core transformer called"))
}

var patternMacroUseList = common.MustCompileSimplePattern(read.MustReadDatum("(keyword _ ...)"))

type CoreExpander struct {
	newMark func() *common.M
}

func NewCoreExpander() CoreExpander {
	return CoreExpander{func() *common.M {
		return common.NewMark()
	}}
}

func NewCoreExpanderWithMarks(marks []common.M) CoreExpander {
	i := 0
	return CoreExpander{func() *common.M {
		m := &marks[i]
		i++
		return m
	}}
}

func (e CoreExpander) Expand(syntax common.Syntax, env common.Environment) (CoreForm, error) {
	switch transformer := MatchTransformer(syntax, env, patternMacroUseList).(type) {
	case CoreTransformer:
		ctx := ExpansionContext{Expander: e, Env: env}
		// TODO: marks should record information about their introducing macro
		mark := e.newMark()
		return ExpandCoreTransformer(ctx, transformer, syntax, mark)
	default:
		if transformer == nil {
			return nil, fmt.Errorf("expand: unhandled syntax %v at %v", common.Write(syntax.Datum()), syntax.SourceLocation())
		}
		return nil, fmt.Errorf("expand: encountered non-core transformer %v", common.Write(transformer))
	}
}

func ExpandCoreTransformer(ctx ExpansionContext, transformer CoreTransformer, syntax common.Syntax, mark *common.M) (CoreForm, error) {
	return transformer.transform(ctx, syntax, mark)
}

func MatchTransformer(syntax common.Syntax, env common.Environment, pattern common.SimplePattern) common.Procedure {
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
