package core_test

import (
	"github.com/katsuya94/grime/common"
	. "github.com/katsuya94/grime/lib/core"
	"github.com/katsuya94/grime/read"
)

func expandNever(Compiler, common.Datum) (common.Datum, bool, error) {
	return nil, false, nil
}

func expressionCompileIdentity(_ Compiler, form common.Datum) (common.Expression, error) {
	return form.(common.Expression), nil
}

// comparableType must be of non-zero size for equality comparison
type comparableType struct {
	*comparableType
}

func (comparableType) Evaluate(c common.Continuation) (common.EvaluationResult, error) {
	return common.CallC(c, common.Void)
}

func comparable() common.Expression {
	c := &comparableType{}
	c.comparableType = c
	return c
}

func data(source string) []common.Datum {
	return read.MustReadData(source)
}

func datum(source string) common.Datum {
	return read.MustReadDatum(source)
}

func wrap(datum common.Datum) common.WrappedSyntax {
	return common.NewWrappedSyntax(datum, nil)
}

func set(datum common.Datum, name common.Symbol, location common.Location) common.Datum {
	scope := common.NewScope()
	scope.Set(common.NewIdentifier(name), location)
	return common.NewSyntax(datum).Push(scope, common.LEXICAL).Form()
}
