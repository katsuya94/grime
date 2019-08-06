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
	patternApplication                 = common.MustCompileSimplePattern(read.MustReadDatum("(procedure arguments ...)"))
)

type BaseExpander struct {
	core *r6rs.CoreExpander
}

// TODO: need better modeling between expanders, maybe an expander struct that implements common methods and gets unique behavior through DI?
func NewBaseExpander() *BaseExpander {
	expander := &BaseExpander{}
	expander.core = r6rs.NewCoreExpander(expander)
	return expander
}

func (e BaseExpander) Expand(syntax common.Syntax, env common.Environment) (r6rs.CoreForm, error) {
	var err error
	for {
		transformer := e.syntacticAbstraction(syntax, env)
		if transformer == nil {
			if _, ok := patternApplication.Match(syntax); ok {
				// TODO: create a SimpleTemplate construct to easily build syntax from simple match results
				syntax = common.NewSyntax(common.Pair{r6rs.ApplicationId.WrappedSyntax, syntax.Datum()})
				continue
			}
			if id, ok := syntax.Identifier(); ok {
				if _, ok := id.Binding(); ok {
					syntax = common.NewSyntax(list(r6rs.ReferenceId.WrappedSyntax, syntax.Datum()))
				} else {
					syntax = common.NewSyntax(list(r6rs.TopId.WrappedSyntax, syntax.Datum()))
				}
				continue
			}
			if wrappedSyntax, ok := syntax.Datum().(common.WrappedSyntax); ok {
				switch wrappedSyntax.Datum().(type) {
				case common.Boolean, common.Number, common.Character, common.String:
					syntax = common.NewSyntax(list(r6rs.LiteralId.WrappedSyntax, syntax.Datum()))
					continue
				}
			}
			return nil, fmt.Errorf("unhandled syntax %v at %v", common.Write(syntax.Datum()), syntax.SourceLocation())
		}
		if coreForm, ok, err := e.core.HandleCoreTransformer(transformer, syntax, env); err != nil {
			return nil, err
		} else if ok {
			return coreForm, nil
		}
		syntax, err = e.core.ApplyTransformer(transformer, syntax)
		if err != nil {
			return nil, err
		}
	}
}

func (e BaseExpander) syntacticAbstraction(syntax common.Syntax, env common.Environment) common.Procedure {
	var transformer common.Procedure
	transformer = e.core.MatchTransformer(syntax, env, patternMacroUseSet)
	if transformer != nil {
		return transformer
	}
	transformer = e.core.MatchTransformer(syntax, env, patternMacroUseList)
	if transformer != nil {
		return transformer
	}
	transformer = e.core.MatchTransformer(syntax, env, patternMacroUseImproperList)
	if transformer != nil {
		return transformer
	}
	transformer = e.core.MatchTransformer(syntax, env, patternMacroUseSingletonIdentifier)
	if transformer != nil {
		return transformer
	}
	return nil
}
