package common

import (
	"fmt"
)

var ErrUnexpectedFinalForm = fmt.Errorf("unexpected final form")

type Expression interface {
	Evaluate(Continuation) (Evaluation, error)
}

type Continuation interface {
	Call(Datum) (Evaluation, error)
}

type Evaluation interface {
	Escape(EscapeEvaluated) (Datum, bool)
	Do() (Evaluation, error)
}

type EvaluateExpression struct {
	continuation Continuation
	expression   Expression
}

func (e EvaluateExpression) Escape(EscapeEvaluated) (Datum, bool) {
	return nil, false
}

func (e EvaluateExpression) Do() (Evaluation, error) {
	return e.expression.Evaluate(e.continuation)
}

type CallContinuation struct {
	continuation Continuation
	value        Datum
}

func (e CallContinuation) Escape(escape EscapeEvaluated) (Datum, bool) {
	c, ok := e.continuation.(EscapeEvaluated)
	if !ok || c != escape {
		return nil, false
	}
	return e.value, true
}

func (e CallContinuation) Do() (Evaluation, error) {
	return e.continuation.Call(e.value)
}

func EvalC(c Continuation, expression Expression) (Evaluation, error) {
	return EvaluateExpression{c, expression}, nil
}

func CallC(c Continuation, value Datum) (Evaluation, error) {
	return CallContinuation{c, value}, nil
}

func ErrorC(err error) (Evaluation, error) {
	return nil, err
}

// Evaluate evaluates an expression with a continuation that will escape. If the continuation is called outside the escaping context, it will error.
func Evaluate(expression Expression) (Datum, error) {
	return WithEscape(func(escape Continuation) (Evaluation, error) {
		return expression.Evaluate(escape)
	})
}

type escapeId struct {
	self *escapeId
}

type EscapeEvaluated struct {
	id *escapeId
}

func NewEscapeEvaluated() EscapeEvaluated {
	id := &escapeId{}
	id.self = id
	return EscapeEvaluated{id}
}

func (EscapeEvaluated) Call(d Datum) (Evaluation, error) {
	return nil, fmt.Errorf("evaluate: escape continuation called without escaping context")
}

// WithEscape calls a function returning an EvaluationResult with a escape continuation. It continues to perform further evaluation or call continuations as necessary for the EvaluationResult.
func WithEscape(f func(Continuation) (Evaluation, error)) (Datum, error) {
	escape := NewEscapeEvaluated()
	evaluation, err := f(escape)
	for {
		if err != nil {
			return nil, err
		}
		value, ok := evaluation.Escape(escape)
		if ok {
			return value, nil
		}
		evaluation, err = evaluation.Do()
	}
}
