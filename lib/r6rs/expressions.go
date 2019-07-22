package r6rs

import (
	"fmt"

	"github.com/katsuya94/grime/common"
)

// Literal evaluates to a literal value.
type Literal struct {
	Datum common.Datum
}

func (e Literal) Evaluate(c common.Continuation, s common.Stack) (common.Evaluation, error) {
	return common.CallC(c, e.Datum)
}

// Reference evaluates to the value of its variable.
type Reference struct {
	Identifier        common.Identifier
	Variable          *common.Variable
	VariableReference common.StackFrameReference
}

func (e Reference) Evaluate(c common.Continuation, s common.Stack) (common.Evaluation, error) {
	var value common.Datum = s.Get(e.VariableReference)
	if value == nil {
		return common.ErrorC(fmt.Errorf("evaluate: %v at %v: cannot reference identifier before its definition", e.Identifier.Name(), e.Identifier.SourceLocation()))
	}
	return common.CallC(c, value)
}

// Lambda evaluates to a closure.
type Lambda struct {
	FrameTemplate      common.FrameTemplate
	Variables          []*common.Variable
	VariableReferences []common.StackFrameReference
	Body               common.Expression
}

func (e Lambda) Evaluate(c common.Continuation, s common.Stack) (common.Evaluation, error) {
	return common.CallC(c, common.Closure{s, e.FrameTemplate, e.Variables, e.VariableReferences, e.Body})
}
