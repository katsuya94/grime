package r6rs

import (
	"fmt"

	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/read"
)

var CoreScope = common.NewScope()
var CoreEnvironment = common.NewEnvironment()

var (
	LiteralId     = coreDefinition(common.Symbol("#%literal"), NewCoreTransformer(transformLiteral))
	ReferenceId   = coreDefinition(common.Symbol("#%reference"), NewCoreTransformer(transformReference))
	LambdaId      = coreDefinition(common.Symbol("#%lambda"), NewCoreTransformer(transformLambda))
	ApplicationId = coreDefinition(common.Symbol("#%application"), NewCoreTransformer(transformApplication))
	TopId         = coreDefinition(common.Symbol("#%top"), NewCoreTransformer(transformTop))
)

func newCoreIdentifier(name common.Symbol) common.Identifier {
	return Introduce(common.NewSyntax(common.NewIdentifier(name).WrappedSyntax)).IdentifierOrDie()
}

func coreDefinition(name common.Symbol, transformer common.Procedure) common.Identifier {
	id, binding := common.Bind(common.NewIdentifier(name), CoreScope, 0)
	(&CoreEnvironment).Extend(binding, common.NewSyntacticAbstraction(transformer))
	return id
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
	binding := id.Binding()
	if binding == nil {
		return nil, fmt.Errorf("unbound identifier: %v at %v", id.Name(), id.SourceLocation())
	}
	if role := id.Role(ctx.Env); role == nil {
		return nil, fmt.Errorf("out of context: %v at %v", id.Name(), id.SourceLocation())
	} else if _, ok := role.(common.Variable); !ok {
		return nil, fmt.Errorf("not a variable: %v at %v", id.Name(), id.SourceLocation())
	}
	return ReferenceForm{binding.Identifier()}, nil
}

var patternLambda = common.MustCompileSimplePattern(read.MustReadDatum("(#%lambda (formals ...) inner)"))

func transformLambda(ctx ExpansionContext, syntax common.Syntax) (CoreForm, error) {
	result, ok := patternLambda.Match(syntax)
	if !ok {
		return nil, fmt.Errorf("#%%lambda: bad syntax")
	}
	phase := result[common.Symbol("#%lambda")].(common.Syntax).IdentifierOrDie().Phase()
	scope := common.NewScope()
	var formals []common.Identifier
	for _, formal := range result[common.Symbol("formals")].([]interface{}) {
		id, ok := formal.(common.Syntax).Identifier()
		if !ok {
			return nil, fmt.Errorf("#%%lambda: bad syntax")
		}
		id, binding := common.Bind(id, scope, phase)
		(&ctx.Env).Extend(binding, common.NewVariable())
		formals = append(formals, id)
	}
	if common.DuplicateIdentifiers(formals...) {
		return nil, fmt.Errorf("#%%lambda: bad syntax")
	}
	innerSyntax := result[common.Symbol("inner")].(common.Syntax)
	innerSyntax = innerSyntax.Push(scope, phase)
	inner, err := ctx.Expand(innerSyntax)
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
