package common

type EvaluationResult interface{}

type FurtherEvaluation struct {
	Environment Environment
	Expression  Datum
}

type ContinuationCall struct {
	Continuation Continuation
	Value        Datum
}
