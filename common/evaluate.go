package common

import (
	"fmt"
)

var ErrUnexpectedFinalForm = fmt.Errorf("unexpected final form")

type Expression interface {
	Evaluate(Continuation) (EvaluationResult, error)
}

type Continuation interface {
	Call(Datum) (EvaluationResult, error)
}

type EvaluationResult interface{}

type FurtherEvaluation struct {
	Continuation Continuation
	Expression   Expression
}

type ContinuationCall struct {
	Continuation Continuation
	Value        Datum
}

func EvalC(c Continuation, expression Expression) (EvaluationResult, error) {
	return FurtherEvaluation{c, expression}, nil
}

func CallC(c Continuation, value Datum) (EvaluationResult, error) {
	return ContinuationCall{c, value}, nil
}

func ErrorC(err error) (EvaluationResult, error) {
	return nil, err
}

// EvaluateOnce evaluates an expression with a continuation that will escape. If the continuation is called outside the escaping context, it will error.
func EvaluateOnce(expression Expression) (Datum, error) {
	return WithEscape(func(escape Continuation) (EvaluationResult, error) {
		return expression.Evaluate(escape)
	})
}

type escapeEvaluated struct{}

func (escapeEvaluated) Call(d Datum) (EvaluationResult, error) {
	return nil, fmt.Errorf("evaluate: escape continuation called without escaping context")
}

// WithEscape calls a function returning an EvaluationResult with a escape continuation. It continues to perform further evaluation or call continuations as necessary for the EvaluationResult.
func WithEscape(f func(Continuation) (EvaluationResult, error)) (Datum, error) {
	escape := &escapeEvaluated{}
	evaluationResult, err := f(escape)
	if err != nil {
		return nil, err
	}
	for {
		switch v := evaluationResult.(type) {
		case FurtherEvaluation:
			evaluationResult, err = v.Expression.Evaluate(v.Continuation)
			if err != nil {
				return nil, err
			}
		case ContinuationCall:
			if v.Continuation == escape {
				return v.Value, nil
			}
			evaluationResult, err = v.Continuation.Call(v.Value)
			if err != nil {
				return nil, err
			}
		default:
			return nil, fmt.Errorf("evaluate: unhandled evaluation result: %#v", v)
		}
	}
}
