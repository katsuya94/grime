package r6rs_base

import (
	"fmt"

	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/lib/r6rs"
	"github.com/katsuya94/grime/read"
)

var BaseScope = common.NewScope()
var BaseEnvironment = r6rs.CoreEnvironment

// TODO: quote should be used directly from core
var (
	setId          = baseDefinition(common.Symbol("set!"), setTransformer)
	lambdaId       = baseDefinition(common.Symbol("lambda"), lambdaTransformer)
	beginId        = baseDefinition(common.Symbol("begin"), beginTransformer)
	defineSyntaxId = baseDefinition(common.Symbol("define-syntax"), defineSyntaxTransformer)
	quoteId        = coreDefinition(common.Symbol("quote"))
	syntaxId       = coreDefinition(common.Symbol("syntax"))
)

func baseDefinition(name common.Symbol, transformer common.Procedure) common.Identifier {
	id, binding := common.Bind(common.NewIdentifier(name), BaseScope, 0)
	(&BaseEnvironment).Extend(binding, common.NewSyntacticAbstraction(transformer))
	return id
}

func coreDefinition(name common.Symbol) common.Identifier {
	coreId := r6rs.Introduce(common.NewSyntax(common.NewIdentifier(name).WrappedSyntax)).IdentifierOrDie()
	binding := coreId.Binding()
	role := r6rs.CoreEnvironment.Lookup(binding)
	id := common.Rebind(common.NewIdentifier(name), binding, BaseScope, 0)
	(&BaseEnvironment).Extend(binding, role)
	return id
}

func Introduce(syntax common.Syntax) common.Syntax {
	return syntax.Push(BaseScope, 0)
}

// TODO: create a SimpleTemplate construct to easily build syntax from simple match results
// it should also support easy marking of transformer introduced syntax, since its hard to enforce otherwise

var applicationTransformer = newBaseTransformer(transformApplication)
var patternApplication = common.MustCompileSimplePattern(read.MustReadDatum("(proc args ...)"))

func transformApplication(ctx r6rs.ExpansionContext, syntax common.Syntax, mark *common.M) (r6rs.CoreForm, error) {
	result, ok := patternApplication.Match(syntax)
	if !ok {
		return nil, fmt.Errorf("bad syntax %v at %v", common.Write(syntax.Datum()), syntax.SourceLocation())
	}
	procedure := result[common.Symbol("proc")].(common.Syntax)
	arguments := make([]common.Syntax, len(result[common.Symbol("args")].([]interface{})))
	for i, syntax := range result[common.Symbol("args")].([]interface{}) {
		arguments[i] = syntax.(common.Syntax)
	}
	output := common.Pair{r6rs.ApplicationId.Mark(mark).WrappedSyntax, common.Pair{procedure.Datum(), list(syntaxDatumSlice(arguments)...)}}
	return ctx.Expand(common.NewSyntax(output))
}

var idTransformer = newBaseTransformer(transformId)

func transformId(ctx r6rs.ExpansionContext, syntax common.Syntax, mark *common.M) (r6rs.CoreForm, error) {
	id, ok := syntax.Identifier()
	if !ok {
		return nil, fmt.Errorf("bad syntax %v at %v", common.Write(syntax.Datum()), syntax.SourceLocation())
	}
	var output common.Datum
	if binding := id.Binding(); binding != nil {
		if role := ctx.Env.Lookup(binding); role == nil {
			return nil, fmt.Errorf("out of context: %v at %v", id.Name(), id.SourceLocation())
		} else if _, ok := role.(common.Variable); !ok {
			return nil, fmt.Errorf("non-variable identifier in expression context: %v is %v at %v", id.Name(), role.Description(), id.SourceLocation())
		}
		output = list(r6rs.LoadId.Mark(mark).WrappedSyntax, binding.Identifier().WrappedSyntax)
	} else {
		output = list(r6rs.TopId.Mark(mark).WrappedSyntax, syntax.Datum())
	}
	return ctx.Expand(common.NewSyntax(output))
}

var literalTransformer = newBaseTransformer(transformLiteral)

func transformLiteral(ctx r6rs.ExpansionContext, syntax common.Syntax, mark *common.M) (r6rs.CoreForm, error) {
	wrappedSyntax, ok := syntax.Datum().(common.WrappedSyntax)
	if !ok {
		return nil, fmt.Errorf("bad syntax %v at %v", common.Write(syntax.Datum()), syntax.SourceLocation())
	}
	switch wrappedSyntax.Datum().(type) {
	case common.Boolean, common.Number, common.Character, common.String:
		output := list(quoteId.Mark(mark).WrappedSyntax, syntax.Datum())
		return ctx.Expand(common.NewSyntax(output))
	}
	return nil, fmt.Errorf("bad syntax %v at %v", common.Write(syntax.Datum()), syntax.SourceLocation())
}

var setTransformer = newBaseTransformer(transformSet)
var patternSet = common.MustCompileSimplePattern(read.MustReadDatum("(set! id value)"))

func transformSet(ctx r6rs.ExpansionContext, syntax common.Syntax, mark *common.M) (r6rs.CoreForm, error) {
	// TODO: implement
	panic("not implemented")
}

var lambdaTransformer = newBaseTransformer(transformLambda)
var patternLambda = common.MustCompileSimplePattern(read.MustReadDatum("(lambda (formals ...) body ...)"))

func transformLambda(ctx r6rs.ExpansionContext, syntax common.Syntax, mark *common.M) (r6rs.CoreForm, error) {
	result, ok := patternLambda.Match(syntax)
	if !ok {
		return nil, fmt.Errorf("%v: bad syntax", syntaxKeywordForErrMsg(syntax))
	}
	scope := common.NewScope()
	phase := result[common.Symbol("lambda")].(common.Syntax).IdentifierOrDie().Phase()
	formals := make([]common.Identifier, len(result[common.Symbol("formals")].([]interface{})))
	for i, formal := range result[common.Symbol("formals")].([]interface{}) {
		id, ok := formal.(common.Syntax).Identifier()
		if !ok {
			return nil, fmt.Errorf("%v: bad syntax", syntaxKeywordForErrMsg(syntax))
		}
		id, binding := common.Bind(id, scope, phase)
		formals[i] = id
		(&ctx.Env).Extend(binding, common.NewVariable())
	}
	if common.DuplicateIdentifiers(formals...) {
		return nil, fmt.Errorf("%v: bad syntax", syntaxKeywordForErrMsg(syntax))
	}
	forms := make([]common.Syntax, len(result[common.Symbol("body")].([]interface{})))
	for i, form := range result[common.Symbol("body")].([]interface{}) {
		forms[i] = form.(common.Syntax).Push(scope, phase)
	}
	output := common.Pair{r6rs.LambdaId.Mark(mark).WrappedSyntax, common.Pair{list(idDatumSlice(formals)...), list(common.Pair{beginId.Mark(mark).WrappedSyntax, list(syntaxDatumSlice(forms)...)})}}
	return ctx.Expand(common.NewSyntax(output))
}
