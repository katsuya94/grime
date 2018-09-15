package eval

import (
	"github.com/katsuya94/grime/common"
)

type applicationProcedureEvaluated struct {
	env       common.Environment
	arguments []common.Datum
}

func (c applicationProcedureEvaluated) Call(d common.Datum) (common.EvaluationResult, error) {
	return applicationEvaluationResult(c.env, d, nil, c.arguments)
}

type applicationArgumentEvaluated struct {
	env                common.Environment
	concreteProcedure  common.Datum
	concreteArguments  []common.Datum
	remainingArguments []common.Datum
}

func (c applicationArgumentEvaluated) Call(d common.Datum) (common.EvaluationResult, error) {
	return applicationEvaluationResult(
		c.env,
		c.concreteProcedure,
		append(c.concreteArguments, d),
		c.remainingArguments,
	)
}

func applicationEvaluationResult(
	env common.Environment,
	concreteProcedure common.Datum,
	concreteArguments []common.Datum,
	remainingArguments []common.Datum,
) (common.EvaluationResult, error) {
	if len(remainingArguments) == 0 {
		p, ok := concreteProcedure.(common.Procedure)
		if !ok {
			return nil, Errorf("application: non-procedure in procedure position")
		}
		return p(env.Empty(), concreteArguments...)
	}
	return common.FurtherEvaluation{
		env.SetContinuation(applicationArgumentEvaluated{
			env,
			concreteProcedure,
			concreteArguments,
			remainingArguments[1:],
		}),
		remainingArguments[0],
	}, nil
}
