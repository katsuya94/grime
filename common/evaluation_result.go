package common

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
	return ContinuationCall{}, err
}
