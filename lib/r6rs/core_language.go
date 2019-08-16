package r6rs

import (
	"fmt"

	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/read"
)

var CoreScope = common.NewScope()
var CoreEnvironment = common.NewEnvironment()

var (
	LiteralId     = coreDefinition(common.Symbol("#%literal"), CoreTransformer{transformLiteral})
	LoadId        = coreDefinition(common.Symbol("#%load"), CoreTransformer{transformLoad})
	LambdaId      = coreDefinition(common.Symbol("#%lambda"), CoreTransformer{transformLambda})
	ApplicationId = coreDefinition(common.Symbol("#%application"), CoreTransformer{transformApplication})
	TopId         = coreDefinition(common.Symbol("#%top"), CoreTransformer{transformTop})
	SequenceId    = coreDefinition(common.Symbol("#%sequence"), CoreTransformer{transformSequence})
)

func coreDefinition(name common.Symbol, transformer common.Procedure) common.Identifier {
	id, binding := common.Bind(common.NewIdentifier(name), CoreScope, 0)
	(&CoreEnvironment).Extend(binding, common.NewSyntacticAbstraction(transformer))
	return id
}

func Introduce(syntax common.Syntax) common.Syntax {
	return syntax.Push(CoreScope, 0)
}

var patternLiteral = common.MustCompileSimplePattern(read.MustReadDatum("(#%literal datum)"))

func transformLiteral(ctx ExpansionContext, syntax common.Syntax, mark *common.M) (CoreForm, error) {
	result, ok := patternLiteral.Match(syntax)
	if !ok {
		return nil, fmt.Errorf("#%%literal: bad syntax")
	}
	datum := result[common.Symbol("datum")].(common.Syntax).Unwrap()
	return LiteralForm{datum, mark}, nil
}

var patternLoad = common.MustCompileSimplePattern(read.MustReadDatum("(#%load id)"))

func transformLoad(ctx ExpansionContext, syntax common.Syntax, mark *common.M) (CoreForm, error) {
	result, ok := patternLoad.Match(syntax)
	if !ok {
		return nil, fmt.Errorf("#%%load: bad syntax")
	}
	id, ok := result[common.Symbol("id")].(common.Syntax).Identifier()
	if !ok {
		return nil, fmt.Errorf("#%%load: bad syntax")
	}
	return LoadForm{id, mark}, nil
}

var patternLambda = common.MustCompileSimplePattern(read.MustReadDatum("(#%lambda (formals ...) inner)"))

func transformLambda(ctx ExpansionContext, syntax common.Syntax, mark *common.M) (CoreForm, error) {
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
	innerSyntax := result[common.Symbol("inner")].(common.Syntax)
	inner, err := ctx.Expand(innerSyntax)
	if err != nil {
		return nil, err
	}
	return LambdaForm{formals, inner, mark}, nil
}

var patternApplication = common.MustCompileSimplePattern(read.MustReadDatum("(#%application proc args ...)"))

func transformApplication(ctx ExpansionContext, syntax common.Syntax, mark *common.M) (CoreForm, error) {
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
	return ApplicationForm{proc, args, mark}, nil
}

var patternTop = common.MustCompileSimplePattern(read.MustReadDatum("(#%top id)"))

func transformTop(ctx ExpansionContext, syntax common.Syntax, mark *common.M) (CoreForm, error) {
	result, ok := patternTop.Match(syntax)
	if !ok {
		return nil, fmt.Errorf("#%%top: bad syntax")
	}
	id, ok := result[common.Symbol("id")].(common.Syntax).Identifier()
	if !ok {
		return nil, fmt.Errorf("#%%top: bad syntax")
	}
	return TopForm{id, mark}, nil
}

var patternSequence = common.MustCompileSimplePattern(read.MustReadDatum("(#%sequence forms ...)"))

func transformSequence(ctx ExpansionContext, syntax common.Syntax, mark *common.M) (CoreForm, error) {
	var err error
	result, ok := patternSequence.Match(syntax)
	if !ok {
		return nil, fmt.Errorf("#%%sequence: bad syntax")
	}
	formsResults := result[common.Symbol("forms")].([]interface{})
	forms := make([]CoreForm, len(formsResults))
	for i := range formsResults {
		forms[i], err = ctx.Expand(formsResults[i].(common.Syntax))
		if err != nil {
			return nil, err
		}
	}
	return SequenceForm{forms, mark}, nil
}
