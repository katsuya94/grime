package r6rs

import (
	"fmt"

	"github.com/katsuya94/grime/common"
)

// Literal evaluates to a literal value.
type Literal struct {
	datum common.Datum
}

func NewLiteral(datum common.Datum) Literal {
	return Literal{datum}
}

func (e Literal) Evaluate(ctx common.EvaluationContext, c common.Continuation) (common.Evaluation, error) {
	return common.CallC(c, e.datum)
}

// Reference evaluates to the value of its variable.
type Reference struct {
	index int
	id    common.Identifier
}

func (e Reference) Evaluate(ctx common.EvaluationContext, c common.Continuation) (common.Evaluation, error) {
	value := ctx.Get(e.index).(common.Variable).Get()
	if value == nil {
		return common.ErrorC(fmt.Errorf("evaluate: %v at %v: cannot reference identifier before its definition", e.id.Name(), e.id.SourceLocation()))
	}
	return common.CallC(c, value)
}

// Lambda evaluates to a closure.
type Lambda struct {
	inner common.Expression
	argn  int
}

func (e Lambda) Evaluate(ctx common.EvaluationContext, c common.Continuation) (common.Evaluation, error) {
	return common.CallC(c, common.NewFunction(e.inner, e.argn))
}

// Application evaluates the application of a procedure do its arguments.
type Application struct {
	Procedure common.Expression
	Arguments []common.Expression
}

func (e Application) Evaluate(ctx common.EvaluationContext, c common.Continuation) (common.Evaluation, error) {
	return common.EvalC(
		ctx,
		applicationProcedureEvaluated{ctx, c, e.Arguments},
		e.Procedure,
	)
}

type applicationProcedureEvaluated struct {
	ctx          common.EvaluationContext
	continuation common.Continuation
	arguments    []common.Expression
}

func (c applicationProcedureEvaluated) Call(d common.Datum) (common.Evaluation, error) {
	return partialEvaluationResult(c.ctx, c.continuation, d, nil, c.arguments)
}

type applicationArgumentEvaluated struct {
	ctx          common.EvaluationContext
	continuation common.Continuation
	procedureV   common.Datum
	argumentsV   []common.Datum
	arguments    []common.Expression
}

func (c applicationArgumentEvaluated) Call(d common.Datum) (common.Evaluation, error) {
	return partialEvaluationResult(
		c.ctx,
		c.continuation,
		c.procedureV,
		append(c.argumentsV, d),
		c.arguments,
	)
}

func partialEvaluationResult(ctx common.EvaluationContext, c common.Continuation, procedureV common.Datum, argumentsV []common.Datum, arguments []common.Expression) (common.Evaluation, error) {
	if len(arguments) == 0 {
		return common.Apply(c, procedureV, argumentsV...)
	}
	return common.EvalC(
		ctx,
		applicationArgumentEvaluated{
			ctx,
			c,
			procedureV,
			argumentsV,
			arguments[1:],
		},
		arguments[0],
	)
}
