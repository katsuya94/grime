package r6rs_base

import (
	"fmt"

	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/lib/r6rs"
	"github.com/katsuya94/grime/read"
	"github.com/katsuya94/grime/runtime"
)

var Library = runtime.MustNewLibraryFromFile("r6rs_base")

var BaseScope = common.NewScope()
var BaseEnvironment = r6rs.CoreEnvironment

var (
	setId          = baseSyntax(common.Symbol("set!"), setTransformer)
	lambdaId       = baseSyntax(common.Symbol("lambda"), lambdaTransformer)
	beginId        = baseSyntax(common.Symbol("begin"), beginTransformer)
	defineSyntaxId = baseSyntax(common.Symbol("define-syntax"), defineSyntaxTransformer)
	quoteId        = coreSyntax(common.Symbol("quote"))
	ifId           = coreSyntax(common.Symbol("if"))
	syntaxId       = baseSyntax(common.Symbol("syntax"), syntaxTransformer)
	syntaxCaseId   = baseSyntax(common.Symbol("syntax-case"), syntaxCaseTransformer)
	underscoreId   = baseSyntax(common.Symbol("_"), common.UnderscoreTransformer)
	ellipsisId     = baseSyntax(common.Symbol("..."), common.EllipsisTransformer)
)

func baseSyntax(name common.Symbol, transformer common.Procedure) common.Identifier {
	id, binding := common.Bind(common.NewIdentifier(name), BaseScope)
	role := common.NewSyntacticAbstraction(transformer)
	(&BaseEnvironment).Extend(binding, role)
	portable := common.NewSyntacticAbstractionPortable(transformer)
	Library.Builtin(name, portable, 0)
	return id
}

func coreSyntax(name common.Symbol) common.Identifier {
	coreId := r6rs.Introduce(common.NewSyntax(common.NewIdentifier(name).WrappedSyntax)).IdentifierOrDie()
	coreBinding := coreId.Binding()
	role := r6rs.CoreEnvironment.Lookup(coreBinding)
	id, binding := common.Bind(common.NewIdentifier(name), BaseScope)
	(&BaseEnvironment).Extend(binding, role)
	portable := common.NewSyntacticAbstractionPortable(role.(common.SyntacticAbstraction).Transformer)
	Library.Builtin(name, portable, 0)
	return id
}

func Introduce(syntax common.Syntax) common.Syntax {
	return syntax.Push(BaseScope)
}

// TODO: create a SimpleTemplate construct to easily build syntax from simple match results
// it should also support easy marking of transformer introduced syntax, since its hard to enforce otherwise

// TODO: move transformers and specs into individual files

// TODO: the relationshop between core and base is slightly different than that of a library using the syntax of another library. Base uses identifiers bound in core's scope directly whereas libraries would use identifiers bound in their own scope, but pointing to transformers exported from another library. The issue being that since these transformers are written in go, they can't access scopes introduced by the library system. Further thought may be required to decouple core and base further.

var applicationTransformer = r6rs.NewCoreTransformer(transformApplication)
var patternApplication = common.MustCompileSimplePattern(read.MustReadDatum("(proc args ...)"))

func transformApplication(ctx common.ExpansionContext, syntax common.Syntax, mark *common.M) (common.CoreForm, error) {
	result, ok := patternApplication.Match(syntax)
	if !ok {
		return nil, fmt.Errorf("bad syntax %v at %v", common.Write(syntax.Datum()), syntax.SourceLocation())
	}
	err := checkInContext(ctx.Env, r6rs.ApplicationId, syntax)
	if err != nil {
		return nil, err
	}
	procedure := result[common.Symbol("proc")].(common.Syntax)
	arguments := make([]common.Syntax, len(result[common.Symbol("args")].([]interface{})))
	for i, syntax := range result[common.Symbol("args")].([]interface{}) {
		arguments[i] = syntax.(common.Syntax)
	}
	output := common.Pair{r6rs.ApplicationId.Mark(mark).WrappedSyntax, common.Pair{procedure.Datum(), list(syntaxDatumSlice(arguments)...)}}
	return ctx.Expander.Expand(ctx, common.NewSyntax(output))
}

var idTransformer = r6rs.NewCoreTransformer(transformId)

func transformId(ctx common.ExpansionContext, syntax common.Syntax, mark *common.M) (common.CoreForm, error) {
	id, ok := syntax.Identifier()
	if !ok {
		return nil, fmt.Errorf("bad syntax %v at %v", common.Write(syntax.Datum()), syntax.SourceLocation())
	}
	err := checkInContext(ctx.Env, r6rs.LoadId, syntax)
	if err != nil {
		return nil, err
	}
	err = checkInContext(ctx.Env, r6rs.TopId, syntax)
	if err != nil {
		return nil, err
	}
	var output common.Datum
	if binding := id.Binding(); binding != nil {
		if role := ctx.Env.Lookup(binding); role == nil {
			return nil, fmt.Errorf("out of context: %v at %v", id.Name(), id.SourceLocation())
		} else if _, ok := role.(common.Variable); !ok {
			return nil, fmt.Errorf("non-variable identifier in expression context: %v is %T at %v", id.Name(), role, id.SourceLocation())
		}
		output = list(r6rs.LoadId.Mark(mark).WrappedSyntax, binding.Identifier().WrappedSyntax)
	} else {
		output = list(r6rs.TopId.Mark(mark).WrappedSyntax, syntax.Datum())
	}
	return ctx.Expander.Expand(ctx, common.NewSyntax(output))
}

var literalTransformer = r6rs.NewCoreTransformer(transformLiteral)

func transformLiteral(ctx common.ExpansionContext, syntax common.Syntax, mark *common.M) (common.CoreForm, error) {
	wrappedSyntax, ok := syntax.Datum().(common.WrappedSyntax)
	if !ok {
		return nil, fmt.Errorf("bad syntax %v at %v", common.Write(syntax.Datum()), syntax.SourceLocation())
	}
	err := checkInContext(ctx.Env, quoteId, syntax)
	if err != nil {
		return nil, err
	}
	switch wrappedSyntax.Datum().(type) {
	case common.Boolean, common.Number, common.Character, common.String:
		output := list(quoteId.Mark(mark).WrappedSyntax, syntax.Datum())
		return ctx.Expander.Expand(ctx, common.NewSyntax(output))
	}
	return nil, fmt.Errorf("bad syntax %v at %v", common.Write(syntax.Datum()), syntax.SourceLocation())
}

var setTransformer = r6rs.NewCoreTransformer(transformSet)
var patternSet = common.MustCompileSimplePattern(read.MustReadDatum("(set! id value)"))

func transformSet(ctx common.ExpansionContext, syntax common.Syntax, mark *common.M) (common.CoreForm, error) {
	panic("not implemented")
}

var lambdaTransformer = r6rs.NewCoreTransformer(transformLambda)
var patternLambda = common.MustCompileSimplePattern(read.MustReadDatum("(lambda (formals ...) body ...)"))

func transformLambda(ctx common.ExpansionContext, syntax common.Syntax, mark *common.M) (common.CoreForm, error) {
	result, ok := patternLambda.Match(syntax)
	if !ok {
		return nil, fmt.Errorf("%v: bad syntax", syntaxKeywordForErrMsg(syntax))
	}
	err := checkInContext(ctx.Env, r6rs.LambdaId, syntax)
	if err != nil {
		return nil, err
	}
	err = checkInContext(ctx.Env, r6rs.SequenceId, syntax)
	if err != nil {
		return nil, err
	}
	scope := common.NewScope()
	formals := make([]common.Identifier, len(result[common.Symbol("formals")].([]interface{})))
	for i, formal := range result[common.Symbol("formals")].([]interface{}) {
		id, ok := formal.(common.Syntax).Identifier()
		if !ok {
			return nil, fmt.Errorf("%v: bad syntax", syntaxKeywordForErrMsg(syntax))
		}
		id, binding := common.Bind(id, scope)
		formals[i] = id
		(&ctx.Env).Extend(binding, common.NewVariable())
	}
	if common.DuplicateIdentifiers(formals...) {
		return nil, fmt.Errorf("%v: duplicate formals", syntaxKeywordForErrMsg(syntax))
	}
	forms := make([]common.Syntax, len(result[common.Symbol("body")].([]interface{})))
	for i, form := range result[common.Symbol("body")].([]interface{}) {
		forms[i] = form.(common.Syntax).Push(scope)
	}
	output := common.Pair{r6rs.LambdaId.Mark(mark).WrappedSyntax, common.Pair{list(idDatumSlice(formals)...), list(common.Pair{beginId.Mark(mark).WrappedSyntax, list(syntaxDatumSlice(forms)...)})}}
	return ctx.Expander.Expand(ctx, common.NewSyntax(output))
}

func checkInContext(env common.Environment, id common.Identifier, syntax common.Syntax) error {
	role := env[id.Binding()]
	if role != nil {
		return nil
	}
	if _, ok := role.(common.SyntacticAbstraction); ok {
		return nil
	}
	return fmt.Errorf("no %v syntax in context at %v", id.Name(), syntax.SourceLocation())
}
