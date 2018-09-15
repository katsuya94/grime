package common

type Continuation interface {
	Call(Datum) (EvaluationResult, error)
}
