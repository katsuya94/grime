package r6rs

import (
	"fmt"

	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/read"
)

var CoreScope = common.NewScope()
var CoreEnvironment = common.NewEnvironment()

var (
	LiteralId     = newCoreIdentifier(common.Symbol("#%literal"))
	ReferenceId   = newCoreIdentifier(common.Symbol("#%reference"))
	LambdaId      = newCoreIdentifier(common.Symbol("#%lambda"))
	ApplicationId = newCoreIdentifier(common.Symbol("#%application"))
	TopId         = newCoreIdentifier(common.Symbol("#%top"))
)

func newCoreIdentifier(name common.Symbol) common.Identifier {
	return Introduce(common.NewSyntax(common.NewIdentifier(name).WrappedSyntax)).IdentifierOrDie()
}

var coreDefinitions = []struct {
	id          common.Identifier
	transformer common.Procedure
}{
	{
		LiteralId,
		NewCoreTransformer(transformLiteral),
	},
	{
		ReferenceId,
		NewCoreTransformer(transformReference),
	},
	{
		LambdaId,
		NewCoreTransformer(transformLambda),
	},
	{
		ApplicationId,
		NewCoreTransformer(transformApplication),
	},
	{
		TopId,
		NewCoreTransformer(transformTop),
	},
}

func init() {
	for _, definition := range coreDefinitions {
		binding := common.NewBinding()
		CoreScope.Add(definition.id, binding)
		(&CoreEnvironment).Extend(binding, common.NewSyntacticAbstraction(definition.transformer))
	}
}

func Introduce(syntax common.Syntax) common.Syntax {
	return syntax.Push(CoreScope, 0)
}

var patternLiteral = common.MustCompileSimplePattern(read.MustReadDatum("(#%literal datum)"))

func transformLiteral(ctx ExpansionContext, syntax common.Syntax) (CoreForm, error) {
	result, ok := patternLiteral.Match(syntax)
	if !ok {
		return nil, fmt.Errorf("#%%literal: bad syntax")
	}
	datum := result[common.Symbol("datum")].(common.Syntax).Unwrap()
	return LiteralForm{datum}, nil
}

var patternReference = common.MustCompileSimplePattern(read.MustReadDatum("(#%reference id)"))

func transformReference(ctx ExpansionContext, syntax common.Syntax) (CoreForm, error) {
	result, ok := patternReference.Match(syntax)
	if !ok {
		return nil, fmt.Errorf("#%%reference: bad syntax")
	}
	id, ok := result[common.Symbol("id")].(common.Syntax).Identifier()
	if !ok {
		return nil, fmt.Errorf("#%%reference: bad syntax")
	}
	return ReferenceForm{id}, nil
}

var patternLambda = common.MustCompileSimplePattern(read.MustReadDatum("(#%lambda (formals ...) inner)"))

func transformLambda(ctx ExpansionContext, syntax common.Syntax) (CoreForm, error) {
	result, ok := patternLambda.Match(syntax)
	if !ok {
		return nil, fmt.Errorf("#%%lambda: bad syntax")
	}
	var formals []common.Identifier
	for _, formal := range result[common.Symbol("formals")].([]interface{}) {
		id, ok := formal.(common.Syntax).Identifier()
		if !ok {
			return nil, fmt.Errorf("#%%lambda: bad syntax")
		}
		formals = append(formals, id)
	}
	if common.DuplicateIdentifiers(formals...) {
		return nil, fmt.Errorf("#%%lambda: bad syntax")
	}
	inner, err := ctx.Expand(result[common.Symbol("inner")].(common.Syntax))
	if err != nil {
		return nil, err
	}
	return LambdaForm{formals, inner}, nil
}

var patternApplication = common.MustCompileSimplePattern(read.MustReadDatum("(#%application proc args ...)"))

func transformApplication(ctx ExpansionContext, syntax common.Syntax) (CoreForm, error) {
	result, ok := patternApplication.Match(syntax)
	if !ok {
		return nil, fmt.Errorf("#%%application: bad syntax")
	}
	proc, err := ctx.Expand(result[common.Symbol("proc")].(common.Syntax))
	if err != nil {
		return nil, err
	}
	argsResults := result[common.Symbol("args")].([]interface{})
	args := make([]CoreForm, len(argsResults))
	for i := range argsResults {
		args[i], err = ctx.Expand(argsResults[i].(common.Syntax))
		if err != nil {
			return nil, err
		}
	}
	return ApplicationForm{proc, args}, nil
}

var patternTop = common.MustCompileSimplePattern(read.MustReadDatum("(#%top id)"))

func transformTop(ctx ExpansionContext, syntax common.Syntax) (CoreForm, error) {
	result, ok := patternTop.Match(syntax)
	if !ok {
		return nil, fmt.Errorf("#%%top: bad syntax")
	}
	id, ok := result[common.Symbol("id")].(common.Syntax).Identifier()
	if !ok {
		return nil, fmt.Errorf("#%%top: bad syntax")
	}
	return TopForm{id}, nil
}
