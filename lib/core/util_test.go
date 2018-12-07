package core_test

import (
	"fmt"
	"reflect"
	"testing"

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
	self *comparableType
}

func (comparableType) Evaluate(c common.Continuation) (common.EvaluationResult, error) {
	return common.CallC(c, common.Void)
}

func comparable() common.Expression {
	expression := &comparableType{}
	expression.self = expression
	return expression
}

func data(source string) []common.Datum {
	return read.MustReadString(source)
}

func datum(source string) common.Datum {
	data := data(source)
	if len(data) != 1 {
		panic(fmt.Sprintf("read %v data", len(data)))
	}
	return data[0]
}

func wrap(datum common.Datum) common.WrappedSyntax {
	return common.NewWrappedSyntax(datum)
}

func assertNoError(t *testing.T, err error) {
	if err != nil {
		t.Fatal(err)
	}
}

func assertDeepEquals(t *testing.T, actual, expected interface{}) {
	if !reflect.DeepEqual(actual, expected) {
		t.Fatalf("\nexpected: %#v\n     got: %#v\n", expected, actual)
	}
}

func assertEquals(t *testing.T, actual, expected interface{}) {
	if actual != expected {
		t.Fatalf("\nexpected: %#v\n     got: %#v\n", expected, actual)
	}
}
