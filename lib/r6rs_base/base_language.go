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
	setId    = newBaseIdentifier(common.Symbol("set!"))
	lambdaId = newBaseIdentifier(common.Symbol("lambda"))
	beginId  = newBaseIdentifier(common.Symbol("begin"))
)

func newBaseIdentifier(name common.Symbol) common.Identifier {
	return Introduce(common.NewSyntax(common.NewIdentifier(name).WrappedSyntax)).IdentifierOrDie()
}

var baseDefinitions = []struct {
	id          common.Identifier
	transformer common.Procedure
}{
	{
		lambdaId,
		common.NewNative(transformLambda),
	},
	{
		beginId,
		common.NewNative(transformBegin),
	},
}

func init() {
	for _, definition := range baseDefinitions {
		binding := common.NewBinding()
		BaseScope.Add(definition.id, binding)
		(&BaseEnvironment).Extend(binding, common.NewSyntacticAbstraction(definition.transformer))
	}
}

func Introduce(syntax common.Syntax) common.Syntax {
	return syntax.Push(BaseScope, 0)
}

var patternLambda = common.MustCompileSimplePattern(read.MustReadDatum("(lambda (formals ...) body ...)"))

func transformLambda(c common.Continuation, args ...common.Datum) (common.Evaluation, error) {
	result, ok := patternLambda.Match(common.NewSyntax(args[0]))
	if !ok {
		return common.ErrorC(fmt.Errorf("lambda: bad syntax"))
	}
	phase := result[common.Symbol("lambda")].(common.Syntax).IdentifierOrDie().Phase()
	scope := common.NewScope()
	var formals []common.Identifier
	for _, syntax := range result[common.Symbol("formals")].([]interface{}) {
		id, ok := syntax.(common.Syntax).Identifier()
		if !ok {
			return common.ErrorC(fmt.Errorf("lambda: bad syntax"))
		}
		binding := common.NewBinding()
		scope.Add(id, binding)
		id = id.Push(scope, phase).IdentifierOrDie()
		formals = append(formals, id)
	}
	if common.DuplicateIdentifiers(formals...) {
		return common.ErrorC(fmt.Errorf("lambda: bad syntax"))
	}
	var forms []common.Syntax
	for _, syntax := range result[common.Symbol("body")].([]interface{}) {
		form := syntax.(common.Syntax)
		form = form.Push(scope, phase)
		forms = append(forms, form)
	}
	output := common.Pair{r6rs.LambdaId.WrappedSyntax, common.Pair{list(idDatumSlice(formals)...), list(common.Pair{beginId.WrappedSyntax, list(syntaxDatumSlice(forms)...)})}}
	return common.CallC(c, output)
}

var patternBegin = common.MustCompileSimplePattern(read.MustReadDatum("(begin body ...)"))

func transformBegin(c common.Continuation, args ...common.Datum) (common.Evaluation, error) {
	result, ok := patternBegin.Match(common.NewSyntax(args[0]))
	if !ok {
		return common.ErrorC(fmt.Errorf("begin: bad syntax"))
	}
	var forms []common.Syntax
	for _, syntax := range result[common.Symbol("body")].([]interface{}) {
		form := syntax.(common.Syntax)
		forms = append(forms, form)
	}
	if len(forms) != 1 {
		panic("not implemented")
	}
	return common.CallC(c, forms[0].Datum())
}
