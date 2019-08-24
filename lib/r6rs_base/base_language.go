package r6rs_base

import (
	"fmt"

	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/lib/r6rs"
	"github.com/katsuya94/grime/read"
)

var BaseScope = common.NewScope()
var BaseEnvironment = r6rs.CoreEnvironment

var (
	setId          = baseDefinition(common.Symbol("set!"), &BaseTransformer{transformSet})
	lambdaId       = baseDefinition(common.Symbol("lambda"), &BaseTransformer{transformLambda})
	beginId        = baseDefinition(common.Symbol("begin"), beginTransformer)
	defineSyntaxId = baseDefinition(common.Symbol("define-syntax"), defineSyntaxTransformer)
)

func baseDefinition(name common.Symbol, transformer common.Procedure) common.Identifier {
	id, binding := common.Bind(common.NewIdentifier(name), BaseScope, 0)
	(&BaseEnvironment).Extend(binding, common.NewSyntacticAbstraction(transformer))
	return id
}

func Introduce(syntax common.Syntax) common.Syntax {
	return syntax.Push(BaseScope, 0)
}

// TODO: create a SimpleTemplate construct to easily build syntax from simple match results
// it should also support easy marking of transformer introduced syntax, since its hard to enforce otherwise

var patternApplication = common.MustCompileSimplePattern(read.MustReadDatum("(proc args ...)"))

func transformApplication(ctx r6rs.ExpansionContext, syntax common.Syntax, mark *common.M) (r6rs.CoreForm, error) {
	result, ok := patternApplication.Match(syntax)
	if !ok {
		return nil, fmt.Errorf("(application): bad syntax")
	}
	procedure := result[common.Symbol("proc")].(common.Syntax)
	arguments := make([]common.Syntax, len(result[common.Symbol("args")].([]interface{})))
	for i, syntax := range result[common.Symbol("args")].([]interface{}) {
		arguments[i] = syntax.(common.Syntax)
	}
	output := common.Pair{r6rs.ApplicationId.Mark(mark).WrappedSyntax, common.Pair{procedure.Datum(), syntaxDatumSlice(arguments)}}
	return ctx.Expand(common.NewSyntax(output))
}

func transformId(ctx r6rs.ExpansionContext, syntax common.Syntax, mark *common.M) (r6rs.CoreForm, error) {
	id, ok := syntax.Identifier()
	if !ok {
		return nil, fmt.Errorf("(id): bad syntax")
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

func transformLiteral(ctx r6rs.ExpansionContext, syntax common.Syntax, mark *common.M) (r6rs.CoreForm, error) {
	wrappedSyntax, ok := syntax.Datum().(common.WrappedSyntax)
	if !ok {
		return nil, fmt.Errorf("(literal): bad syntax %v at %v", common.Write(syntax.Datum()), syntax.SourceLocation())
	}
	switch wrappedSyntax.Datum().(type) {
	case common.Boolean, common.Number, common.Character, common.String:
		output := list(r6rs.LiteralId.Mark(mark).WrappedSyntax, syntax.Datum())
		return ctx.Expand(common.NewSyntax(output))
	}
	return nil, fmt.Errorf("(literal): bad syntax %v at %v", common.Write(syntax.Datum()), syntax.SourceLocation())
}

var patternSet = common.MustCompileSimplePattern(read.MustReadDatum("(set! id value)"))

func transformSet(ctx r6rs.ExpansionContext, syntax common.Syntax, mark *common.M) (r6rs.CoreForm, error) {
	// TODO: implement
	panic("not implemented")
}

var patternLambda = common.MustCompileSimplePattern(read.MustReadDatum("(lambda (formals ...) body ...)"))

func transformLambda(ctx r6rs.ExpansionContext, syntax common.Syntax, mark *common.M) (r6rs.CoreForm, error) {
	result, ok := patternLambda.Match(syntax)
	if !ok {
		return nil, fmt.Errorf("lambda: bad syntax")
	}
	scope := common.NewScope()
	phase := result[common.Symbol("lambda")].(common.Syntax).IdentifierOrDie().Phase()
	formals := make([]common.Identifier, len(result[common.Symbol("formals")].([]interface{})))
	for i, syntax := range result[common.Symbol("formals")].([]interface{}) {
		id, ok := syntax.(common.Syntax).Identifier()
		if !ok {
			return nil, fmt.Errorf("lambda: bad syntax")
		}
		id, binding := common.Bind(id, scope, phase)
		formals[i] = id
		(&ctx.Env).Extend(binding, common.NewVariable())
	}
	if common.DuplicateIdentifiers(formals...) {
		return nil, fmt.Errorf("lambda: bad syntax")
	}
	forms := make([]common.Syntax, len(result[common.Symbol("body")].([]interface{})))
	for i, syntax := range result[common.Symbol("body")].([]interface{}) {
		forms[i] = syntax.(common.Syntax).Push(scope, phase)
	}
	output := common.Pair{r6rs.LambdaId.Mark(mark).WrappedSyntax, common.Pair{list(idDatumSlice(formals)...), list(common.Pair{beginId.Mark(mark).WrappedSyntax, list(syntaxDatumSlice(forms)...)})}}
	return ctx.Expand(common.NewSyntax(output))
}

var patternBegin = common.MustCompileSimplePattern(read.MustReadDatum("(begin body ...)"))

func transformBegin(ctx r6rs.ExpansionContext, syntax common.Syntax, mark *common.M) (r6rs.CoreForm, error) {
	result, ok := patternBegin.Match(syntax)
	if !ok {
		return nil, fmt.Errorf("begin: bad syntax")
	}
	forms := make([]common.Syntax, len(result[common.Symbol("body")].([]interface{})))
	for i, syntax := range result[common.Symbol("body")].([]interface{}) {
		forms[i] = syntax.(common.Syntax)
	}
	return expandBody(ctx, forms, mark)
}
