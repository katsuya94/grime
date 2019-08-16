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

// Load evaluates to the value of a Location in the local context.
type Load struct {
	index int
	id    common.Identifier
}

func NewLoad(index int, id common.Identifier) Load {
	return Load{index, id}
}

func (e Load) Evaluate(ctx common.EvaluationContext, c common.Continuation) (common.Evaluation, error) {
	value := ctx.Get(e.index).Get()
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

func NewLambda(inner common.Expression, argn int) Lambda {
	return Lambda{inner, argn}
}

func (e Lambda) Evaluate(ctx common.EvaluationContext, c common.Continuation) (common.Evaluation, error) {
	return common.CallC(c, common.NewFunction(e.inner, e.argn))
}

// Application evaluates the application of a procedure do its arguments.
type Application struct {
	procedure common.Expression
	arguments []common.Expression
}

func NewApplication(procedure common.Expression, arguments ...common.Expression) Application {
	return Application{procedure, arguments}
}

func (e Application) Evaluate(ctx common.EvaluationContext, c common.Continuation) (common.Evaluation, error) {
	return common.EvalC(
		ctx,
		applicationProcedureEvaluated{ctx, c, e.arguments},
		e.procedure,
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

// Top evaluates to the value of a location in the global context.
type Top struct {
	location common.Location
}

func NewTop(location common.Location) Top {
	return Top{location}
}

func (e Top) Evaluate(ctx common.EvaluationContext, c common.Continuation) (common.Evaluation, error) {
	value := e.location.Get()
	if value == nil {
		panic(fmt.Sprintf("undefined %v", e.location))
	}
	return common.CallC(c, value)
}

// Sequence evaluates all of its subexpressions, returning the last.
type Sequence struct {
	expressions []common.Expression
}

func NewSequence(expressions []common.Expression) Sequence {
	return Sequence{expressions}
}

func (e Sequence) Evaluate(ctx common.EvaluationContext, c common.Continuation) (common.Evaluation, error) {
	return common.EvalC(
		ctx,
		sequenceFirstEvaluated{ctx, c, e.expressions[1:]},
		e.expressions[0],
	)
}

type sequenceFirstEvaluated struct {
	ctx          common.EvaluationContext
	continuation common.Continuation
	expressions  []common.Expression
}

func (c sequenceFirstEvaluated) Call(d common.Datum) (common.Evaluation, error) {
	if len(c.expressions) == 0 {
		return common.CallC(c.continuation, d)
	}
	return common.EvalC(
		c.ctx,
		sequenceFirstEvaluated{c.ctx, c.continuation, c.expressions[1:]},
		c.expressions[0],
	)
}
