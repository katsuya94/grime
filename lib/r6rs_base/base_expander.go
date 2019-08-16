package r6rs_base

import (
	"fmt"

	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/lib/r6rs"
	"github.com/katsuya94/grime/read"
)

var (
	patternMacroUseSet                 = common.MustCompileSimplePatternWithIdentifierLiterals(read.MustReadDatum("(set! keyword _)"), setId)
	patternMacroUseList                = common.MustCompileSimplePattern(read.MustReadDatum("(keyword _ ...)"))
	patternMacroUseImproperList        = common.MustCompileSimplePattern(read.MustReadDatum("(keyword _ ... . _)"))
	patternMacroUseSingletonIdentifier = common.MustCompileSimplePattern(read.MustReadDatum("keyword"))
)

type BaseTransformer struct {
	expand func(common.Syntax, *common.M) (common.Syntax, error)
}

func (BaseTransformer) Call(c common.Continuation, args ...common.Datum) (common.Evaluation, error) {
	return common.ErrorC(fmt.Errorf("base transformer called"))
}

type BaseExpander struct{}

func (e BaseExpander) Expand(syntax common.Syntax, env common.Environment) (r6rs.CoreForm, error) {
	var err error
	for {
		mark := common.NewMark()
		switch transformer := matchTransformer(syntax, env).(type) {
		case r6rs.CoreTransformer:
			ctx := r6rs.ExpansionContext{Expander: e, Env: env}
			return r6rs.ExpandCoreTransformer(ctx, transformer, syntax, mark)
		case BaseTransformer:
			syntax, err = ExpandBaseTransformer(transformer, syntax, mark)
			if err != nil {
				return nil, err
			}
		default:
			if transformer == nil {
				if _, ok := patternApplication.Match(syntax); ok {
					syntax, err = ExpandBaseTransformer(BaseTransformer{transformApplication}, syntax, mark)
					if err != nil {
						return nil, err
					}
					continue
				}
				if _, ok := syntax.Identifier(); ok {
					syntax, err = ExpandBaseTransformer(BaseTransformer{transformId}, syntax, mark)
					if err != nil {
						return nil, err
					}
					continue
				}
				syntax, err = ExpandBaseTransformer(BaseTransformer{transformLiteral}, syntax, mark)
				if err != nil {
					return nil, err
				}
				continue
			}
			syntax, err = applyTransformer(transformer, syntax, mark)
			if err != nil {
				return nil, err
			}
		}
	}
}

func ExpandBaseTransformer(transformer BaseTransformer, syntax common.Syntax, mark *common.M) (common.Syntax, error) {
	return transformer.expand(syntax, mark)
}

func matchTransformer(syntax common.Syntax, env common.Environment) common.Procedure {
	var transformer common.Procedure
	transformer = r6rs.MatchTransformer(syntax, env, patternMacroUseSet)
	if transformer != nil {
		return transformer
	}
	transformer = r6rs.MatchTransformer(syntax, env, patternMacroUseList)
	if transformer != nil {
		return transformer
	}
	transformer = r6rs.MatchTransformer(syntax, env, patternMacroUseImproperList)
	if transformer != nil {
		return transformer
	}
	transformer = r6rs.MatchTransformer(syntax, env, patternMacroUseSingletonIdentifier)
	if transformer != nil {
		return transformer
	}
	return nil
}

func applyTransformer(transformer common.Procedure, syntax common.Syntax, mark *common.M) (common.Syntax, error) {
	input := syntax.Mark(mark)
	output, err := common.WithEscape(func(escape common.Continuation) (common.Evaluation, error) {
		return transformer.Call(escape, input.Datum())
	})
	if err != nil {
		return common.Syntax{}, err
	}
	return common.NewSyntax(output).Mark(mark), nil
}
