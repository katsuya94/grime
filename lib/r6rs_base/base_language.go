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
	setId    = baseDefinition(common.Symbol("set!"), BaseTransformer{transformSet})
	lambdaId = baseDefinition(common.Symbol("lambda"), BaseTransformer{transformLambda})
	beginId  = baseDefinition(common.Symbol("begin"), BaseTransformer{transformBegin})
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

var patternApplication = common.MustCompileSimplePattern(read.MustReadDatum("(proc args ...)"))

func transformApplication(syntax common.Syntax, mark *common.M) (common.Syntax, error) {
	result, ok := patternApplication.Match(syntax)
	if !ok {
		return common.Syntax{}, fmt.Errorf("(application): bad syntax")
	}
	procedure := result[common.Symbol("proc")].(common.Syntax)
	arguments := make([]common.Syntax, len(result[common.Symbol("args")].([]interface{})))
	for i, syntax := range result[common.Symbol("args")].([]interface{}) {
		arguments[i] = syntax.(common.Syntax)
	}
	output := common.Pair{r6rs.ApplicationId.WrappedSyntax, common.Pair{procedure.Datum(), syntaxDatumSlice(arguments)}}
	return common.NewSyntax(output), nil
}

func transformId(syntax common.Syntax, mark *common.M) (common.Syntax, error) {
	id, ok := syntax.Identifier()
	if !ok {
		return common.Syntax{}, fmt.Errorf("(id): bad syntax")
	}
	var output common.Datum
	if binding := id.Binding(); binding != nil {
		output = list(r6rs.LoadId.WrappedSyntax, syntax.Datum())
	} else {
		output = list(r6rs.TopId.WrappedSyntax, syntax.Datum())
	}
	return common.NewSyntax(output), nil
}

func transformLiteral(syntax common.Syntax, mark *common.M) (common.Syntax, error) {
	wrappedSyntax, ok := syntax.Datum().(common.WrappedSyntax)
	if !ok {
		return common.Syntax{}, fmt.Errorf("(literal): bad syntax %v at %v", common.Write(syntax.Datum()), syntax.SourceLocation())
	}
	switch wrappedSyntax.Datum().(type) {
	case common.Boolean, common.Number, common.Character, common.String:
		output := list(r6rs.LiteralId.WrappedSyntax, syntax.Datum())
		return common.NewSyntax(output), nil
	}
	return common.Syntax{}, fmt.Errorf("(literal): bad syntax %v at %v", common.Write(syntax.Datum()), syntax.SourceLocation())
}

var patternSet = common.MustCompileSimplePattern(read.MustReadDatum("(set! id value)"))

func transformSet(syntax common.Syntax, mark *common.M) (common.Syntax, error) {
	panic("not implemented")
}

var patternLambda = common.MustCompileSimplePattern(read.MustReadDatum("(lambda (formals ...) body ...)"))

func transformLambda(syntax common.Syntax, mark *common.M) (common.Syntax, error) {
	result, ok := patternLambda.Match(syntax)
	if !ok {
		return common.Syntax{}, fmt.Errorf("lambda: bad syntax")
	}
	formals := make([]common.Identifier, len(result[common.Symbol("formals")].([]interface{})))
	for i, syntax := range result[common.Symbol("formals")].([]interface{}) {
		id, ok := syntax.(common.Syntax).Identifier()
		if !ok {
			return common.Syntax{}, fmt.Errorf("lambda: bad syntax")
		}
		formals[i] = id
	}
	if common.DuplicateIdentifiers(formals...) {
		return common.Syntax{}, fmt.Errorf("lambda: bad syntax")
	}
	forms := make([]common.Syntax, len(result[common.Symbol("body")].([]interface{})))
	for i, syntax := range result[common.Symbol("body")].([]interface{}) {
		forms[i] = syntax.(common.Syntax)
	}
	output := common.Pair{r6rs.LambdaId.WrappedSyntax, common.Pair{list(idDatumSlice(formals)...), list(common.Pair{beginId.WrappedSyntax, list(syntaxDatumSlice(forms)...)})}}
	return common.NewSyntax(output), nil
}

var patternBegin = common.MustCompileSimplePattern(read.MustReadDatum("(begin body ...)"))

func transformBegin(syntax common.Syntax, mark *common.M) (common.Syntax, error) {
	result, ok := patternBegin.Match(syntax)
	if !ok {
		return common.Syntax{}, fmt.Errorf("begin: bad syntax")
	}
	forms := make([]common.Syntax, len(result[common.Symbol("body")].([]interface{})))
	for i, syntax := range result[common.Symbol("body")].([]interface{}) {
		forms[i] = syntax.(common.Syntax)
	}
	output := common.Pair{r6rs.SequenceId.WrappedSyntax, list(syntaxDatumSlice(forms)...)}
	return common.NewSyntax(output), nil
}
