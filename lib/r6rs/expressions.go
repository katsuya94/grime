package r6rs

import (
	"fmt"

	"github.com/katsuya94/grime/common"
)

// Literal evaluates to a literal value.
type Literal struct {
	Datum common.Datum
}

func (e Literal) Evaluate(ctx common.EvaluationContext, c common.Continuation) (common.Evaluation, error) {
	return common.CallC(c, e.Datum)
}

// Reference evaluates to the value of its variable.
type Reference struct {
	Identifier common.Identifier
}

func (e Reference) Evaluate(ctx common.EvaluationContext, c common.Continuation) (common.Evaluation, error) {
	var value common.Datum = s.Get(e.VariableReference)
	if value == nil {
		return common.ErrorC(fmt.Errorf("evaluate: %v at %v: cannot reference identifier before its definition", e.Identifier.Name(), e.Identifier.SourceLocation()))
	}
	return common.CallC(c, value)
}

// Lambda evaluates to a closure.
type Lambda struct {
	Inner common.Expression
}

func (e Lambda) Evaluate(ctx common.EvaluationContext, c common.Continuation) (common.Evaluation, error) {
	return common.CallC(c, common.Closure{s, e.FrameTemplate, e.Variables, e.VariableReferences, e.Body})
}

// Application evaluates the application of a procedure do its arguments.
type Application struct {
	Procedure common.Expression
	Arguments []common.Expression
}

func (e Application) Evaluate(ctx common.EvaluationContext, c common.Continuation) (common.Evaluation, error) {
	return common.CallC(c, common.Closure{s, e.FrameTemplate, e.Variables, e.VariableReferences, e.Body})
}
