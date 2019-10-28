package r6rs_base

import (
	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/read"
)

var (
	patternMacroUseSet                 = common.MustCompileSimplePatternWithIdentifierLiterals(read.MustReadDatum("(set! keyword _)"), setId)
	patternMacroUseList                = common.MustCompileSimplePattern(read.MustReadDatum("(keyword _ ...)"))
	patternMacroUseImproperList        = common.MustCompileSimplePattern(read.MustReadDatum("(keyword _ ... . _)"))
	patternMacroUseSingletonIdentifier = common.MustCompileSimplePattern(read.MustReadDatum("keyword"))
)

type ExpanderFactory struct{}

func (ef ExpanderFactory) Expander(envProvider common.EnvironmentProvider) common.Expander {
	return Expander{envProvider}
}

func Expand(env common.Environment, syntax common.Syntax) (common.CoreForm, error) {
	expander := Expander{common.SingleEnvironmentProvider(env)}
	ctx := common.ExpansionContext{Expander: expander, Env: env, Phase: 0}
	return expander.Expand(ctx, syntax)
}

type Expander struct {
	envProvider common.EnvironmentProvider
}

func (e Expander) Expand(ctx common.ExpansionContext, syntax common.Syntax) (common.CoreForm, error) {
	return expand(ctx, syntax)
}

func (e Expander) ExpandBody(ctx common.ExpansionContext, forms []common.Syntax, scope *common.Scope) (common.CoreForm, common.Environment, error) {
	mark := common.NewMark()
	return expandBody(ctx, forms, mark, scope)
}

func (e Expander) Context(phase int) common.ExpansionContext {
	return common.ExpansionContext{Expander: e, Env: e.envProvider.Environment(phase), Phase: phase}
}
