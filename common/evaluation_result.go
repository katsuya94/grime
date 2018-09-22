package common

type EvaluationResult interface{}

type FurtherEvaluation struct {
	Environment Environment
	Expression  Expression
}

type ContinuationCall struct {
	Continuation Continuation
	Value        Datum
}

func EvalC(env Environment, expression Expression) (EvaluationResult, error) {
	return FurtherEvaluation{env, expression}, nil
}

func CallC(env Environment, value Datum) (EvaluationResult, error) {
	return ContinuationCall{env.Continuation(), value}, nil
}

func ErrorC(err error) (EvaluationResult, error) {
	return ContinuationCall{}, err
}
