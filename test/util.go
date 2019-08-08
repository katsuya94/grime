package test

import (
	"fmt"
	"strings"
	"testing"

	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/read"
	"github.com/pmezard/go-difflib/difflib"
	"github.com/stretchr/testify/assert"
)

// func Bindings(n int) []common.Variable {
// 	frameTemplate := common.NewFrameTemplate()
// 	variables := make([]common.Variable, n)
// 	for i := 0; i < n; i++ {
// 		variables[i] = *common.NewVariable(&frameTemplate)
// 	}
// 	return variables
// }

func Syntax(s string) common.Syntax {
	return common.NewSyntax(common.NewWrappedSyntax(read.MustReadDatum(s), nil))
}

func Identifier(s string) common.Identifier {
	return Syntax(s).IdentifierOrDie()
}

func AssertSyntaxEqual(t *testing.T, expected common.Syntax, actual common.Syntax, msgAndArgs ...interface{}) {
	equal := actual.Equal(expected)
	if equal {
		return
	}
	diff, _ := difflib.GetUnifiedDiffString(difflib.UnifiedDiff{
		A:        difflib.SplitLines(strings.Replace(expected.PrettyPrint(0), "%", "%%", -1)),
		B:        difflib.SplitLines(strings.Replace(actual.PrettyPrint(0), "%", "%%", -1)),
		FromFile: "Expected",
		FromDate: "",
		ToFile:   "Actual",
		ToDate:   "",
		Context:  1,
	})
	msg := fmt.Sprintf("Syntax not equal: \n"+
		"expected: %s\n"+
		"actual  : %s\n"+
		"\n"+
		"Diff:\n"+
		"%s", common.Write(expected.Datum()), common.Write(actual.Datum()), diff)
	assert.Fail(t, msg, msgAndArgs...)
}

func Evaluate(expression common.Expression) (common.Datum, error) {
	return common.Evaluate(common.NewEvaluationContext(), expression)
}

// func WithLiteral(id common.Identifier, syntax common.Syntax) common.Syntax {
// 	return WithBinding(id, &common.Literal{id}, syntax)
// }

// func WithBinding(id common.Identifier, binding common.Binding, syntax common.Syntax) common.Syntax {
// 	scope := common.NewScope()
// 	err := scope.Set(id, binding)
// 	if err != nil {
// 		panic(err)
// 	}
// 	return syntax.Push(scope, common.LEXICAL, false)
// }

// voidExpression must be of non-zero size for equality comparison
type voidExpression struct {
	self *voidExpression
}

func (*voidExpression) Evaluate(ctx common.EvaluationContext, c common.Continuation) (common.Evaluation, error) {
	return common.CallC(c, common.Void)
}

func NewVoidExpression() common.Expression {
	e := &voidExpression{}
	e.self = e
	return e
}

// func Grime(t *testing.T, source string) {
// 	_, file, line, ok := runtime.Caller(1)
// 	if !ok {
// 		panic("can't determine caller")
// 	}
// 	filename := fmt.Sprintf("%v:%v:github.com/katsuya94/grime/test.Grime", file, line)
// 	topLevelProgram, nullSourceLocationTree, err := read.Read(filename, strings.NewReader(source))
// 	require.NoError(t, err)
// 	err = lib.Runtime.Execute(topLevelProgram, nullSourceLocationTree)
// 	require.NoError(t, err)
// }
