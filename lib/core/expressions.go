package core

import (
	"fmt"

	"github.com/katsuya94/grime/common"
)

// Application evaluates its procedure with its arguments.
type Application struct {
	Procedure common.Expression
	Arguments []common.Expression
}

func (e Application) Evaluate(c common.Continuation, s common.Stack) (common.Evaluation, error) {
	return common.EvalC(
		applicationProcedureEvaluated{c, s, e.Arguments},
		s,
		e.Procedure,
	)
}

type applicationProcedureEvaluated struct {
	continuation common.Continuation
	stack        common.Stack
	arguments    []common.Expression
}

func (c applicationProcedureEvaluated) Call(d common.Datum) (common.Evaluation, error) {
	return partialEvaluationResult(c.continuation, c.stack, d, nil, c.arguments)
}

type applicationArgumentEvaluated struct {
	continuation common.Continuation
	stack        common.Stack
	procedureV   common.Datum
	argumentsV   []common.Datum
	arguments    []common.Expression
}

func (c applicationArgumentEvaluated) Call(d common.Datum) (common.Evaluation, error) {
	return partialEvaluationResult(
		c.continuation,
		c.stack,
		c.procedureV,
		append(c.argumentsV, d),
		c.arguments,
	)
}

func partialEvaluationResult(c common.Continuation, s common.Stack, procedureV common.Datum, argumentsV []common.Datum, arguments []common.Expression) (common.Evaluation, error) {
	if len(arguments) == 0 {
		return common.Apply(c, procedureV, argumentsV...)
	}
	return common.EvalC(
		applicationArgumentEvaluated{
			c,
			s,
			procedureV,
			argumentsV,
			arguments[1:],
		},
		s,
		arguments[0],
	)
}

// If evaluates its condition, then evaluates its then or else branches accordingly.
type If struct {
	Condition common.Expression
	Then      common.Expression
	Else      common.Expression
}

func (e If) Evaluate(c common.Continuation, s common.Stack) (common.Evaluation, error) {
	return common.EvalC(
		ifConditionEvaluated{c, s, e.Then, e.Else},
		s,
		e.Condition,
	)
}

type ifConditionEvaluated struct {
	continuation common.Continuation
	stack        common.Stack
	then         common.Expression
	otherwise    common.Expression
}

func (c ifConditionEvaluated) Call(d common.Datum) (common.Evaluation, error) {
	if d == common.Boolean(false) {
		return common.EvalC(c.continuation, c.stack, c.otherwise)
	} else {
		return common.EvalC(c.continuation, c.stack, c.then)
	}
}

// Let evaluates its init expression and assigns it to its variable before evaluating its body.
type Let struct {
	Variable          *common.Variable
	VariableReference common.StackFrameReference
	Init              common.Expression
	Body              common.Expression
}

func (e Let) Evaluate(c common.Continuation, s common.Stack) (common.Evaluation, error) {
	return common.EvalC(
		letInitEvaluated{c, s, e.Variable, e.VariableReference, e.Body},
		s,
		e.Init,
	)
}

type letInitEvaluated struct {
	continuation      common.Continuation
	stack             common.Stack
	variable          *common.Variable
	variableReference common.StackFrameReference
	body              common.Expression
}

func (c letInitEvaluated) Call(d common.Datum) (common.Evaluation, error) {
	c.stack.Set(c.variableReference, d)
	return common.EvalC(
		letBodyEvaluated{c.continuation},
		c.stack,
		c.body,
	)
}

type letBodyEvaluated struct {
	continuation common.Continuation
}

func (c letBodyEvaluated) Call(d common.Datum) (common.Evaluation, error) {
	return common.CallC(c.continuation, d)
}

// Begin evaluates its expressions in order.
type Begin struct {
	Expressions []common.Expression
}

func (e Begin) Evaluate(c common.Continuation, s common.Stack) (common.Evaluation, error) {
	return common.EvalC(
		beginFirstEvaluated{c, s, e.Expressions[1:]},
		s,
		e.Expressions[0],
	)
}

type beginFirstEvaluated struct {
	continuation common.Continuation
	stack        common.Stack
	expressions  []common.Expression
}

func (c beginFirstEvaluated) Call(d common.Datum) (common.Evaluation, error) {
	if len(c.expressions) == 0 {
		return common.CallC(c.continuation, d)
	}
	return common.EvalC(
		beginFirstEvaluated{c.continuation, c.stack, c.expressions[1:]},
		c.stack,
		c.expressions[0],
	)
}

// Define sets its variable to the value of its expression.
type Define struct {
	Variable          *common.Variable
	VariableReference common.StackFrameReference
	Expression        common.Expression
}

func (e Define) Evaluate(c common.Continuation, s common.Stack) (common.Evaluation, error) {
	return common.EvalC(
		defineExpressionEvaluated{c, s, e.Variable, e.VariableReference},
		s,
		e.Expression,
	)
}

type defineExpressionEvaluated struct {
	continuation      common.Continuation
	stack             common.Stack
	variable          *common.Variable
	variableReference common.StackFrameReference
}

func (c defineExpressionEvaluated) Call(d common.Datum) (common.Evaluation, error) {
	if c.stack.Get(c.variableReference) != nil {
		return common.ErrorC(fmt.Errorf("TODO: continuation calls need a new stack frame?"))
	}
	c.stack.Set(c.variableReference, d)
	return common.CallC(c.continuation, common.Void)
}

// Set sets its variable to the value of its expression.
// TODO: rename Set -> Assign
type Set struct {
	Identifier        common.Identifier
	Variable          *common.Variable
	VariableReference common.StackFrameReference
	Expression        common.Expression
}

func (e Set) Evaluate(c common.Continuation, s common.Stack) (common.Evaluation, error) {
	return common.EvalC(
		setExpressionEvaluated{c, s, e.Identifier, e.Variable, e.VariableReference},
		s,
		e.Expression,
	)
}

type setExpressionEvaluated struct {
	continuation      common.Continuation
	stack             common.Stack
	identifier        common.Identifier
	variable          *common.Variable
	variableReference common.StackFrameReference
}

func (c setExpressionEvaluated) Call(d common.Datum) (common.Evaluation, error) {
	if c.stack.Get(c.variableReference) == nil {
		return common.ErrorC(fmt.Errorf("evaluate: %v at %v: cannot set identifier before its definition", c.identifier.Name(), c.identifier.SourceLocation()))
	}
	c.stack.Set(c.variableReference, d)
	return common.CallC(c.continuation, common.Void)
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

// SyntaxCase evaluates its input and evaluates to an output according to pattern matching and fender expressions.
type SyntaxCase struct {
	Input                      common.Expression
	Patterns                   []common.Pattern
	PatternVariabless          [][]*common.PatternVariable
	PatternVariableReferencess [][]common.StackFrameReference
	Fenders                    []common.Expression
	Outputs                    []common.Expression
}

func (e SyntaxCase) Evaluate(c common.Continuation, s common.Stack) (common.Evaluation, error) {
	return common.EvalC(
		syntaxCaseInputEvaluated{c, s, e.Patterns, e.PatternVariabless, e.PatternVariableReferencess, e.Fenders, e.Outputs},
		s,
		e.Input,
	)
}

type syntaxCaseInputEvaluated struct {
	continuation               common.Continuation
	stack                      common.Stack
	patterns                   []common.Pattern
	patternVariabless          [][]*common.PatternVariable
	patternVariableReferencess [][]common.StackFrameReference
	fenders                    []common.Expression
	outputs                    []common.Expression
}

func (c syntaxCaseInputEvaluated) Call(d common.Datum) (common.Evaluation, error) {
	return syntaxCaseMatch(c.continuation, c.stack, d, c.patterns, c.patternVariabless, c.patternVariableReferencess, c.fenders, c.outputs)
}

type syntaxCaseFenderEvaluated struct {
	continuation               common.Continuation
	stack                      common.Stack
	input                      common.Datum
	output                     common.Expression
	patterns                   []common.Pattern
	patternVariabless          [][]*common.PatternVariable
	patternVariableReferencess [][]common.StackFrameReference
	fenders                    []common.Expression
	outputs                    []common.Expression
}

func (c syntaxCaseFenderEvaluated) Call(d common.Datum) (common.Evaluation, error) {
	if d == common.Boolean(false) {
		return syntaxCaseMatch(c.continuation, c.stack, c.input, c.patterns, c.patternVariabless, c.patternVariableReferencess, c.fenders, c.outputs)
	}
	return common.EvalC(c.continuation, c.stack, c.output)
}

func syntaxCaseMatch(c common.Continuation, s common.Stack, input common.Datum, patterns []common.Pattern, patternVariabless [][]*common.PatternVariable, patternVariableReferencess [][]common.StackFrameReference, fenders []common.Expression, outputs []common.Expression) (common.Evaluation, error) {
	syntax := common.NewSyntax(input)
	for i := range patterns {
		result, ok := patterns[i].Match(syntax)
		if ok {
			for j := range patternVariabless[i] {
				s.Set(patternVariableReferencess[i][j], result[patternVariabless[i][j]])
			}
			return common.EvalC(
				syntaxCaseFenderEvaluated{
					c,
					s,
					input,
					outputs[i],
					patterns[i+1:],
					patternVariabless[i+1:],
					patternVariableReferencess[i+1:],
					fenders[i+1:],
					outputs[i+1:],
				},
				s,
				fenders[i],
			)
		}
	}
	return nil, fmt.Errorf("in macro use at %v: bad syntax", syntax.SourceLocation())
}

// SyntaxTemplate evaluates to syntax handling repitition and pattern variable references accordingly.
type SyntaxTemplate struct {
	Template                  common.Datum
	PatternVariables          []*common.PatternVariable
	PatternVariableReferences []common.StackFrameReference
}

func (e SyntaxTemplate) Evaluate(c common.Continuation, s common.Stack) (common.Evaluation, error) {
	bindings := make(map[*common.PatternVariable]interface{}, len(e.PatternVariables))
	for i := range e.PatternVariables {
		bindings[e.PatternVariables[i]] = s.Get(e.PatternVariableReferences[i])
	}
	datum, err := evaluateSyntaxTemplate(e.Template, bindings)
	if err != nil {
		return common.ErrorC(err)
	}
	return common.CallC(c, datum)
}

func evaluateSyntaxTemplate(datum common.Datum, bindings map[*common.PatternVariable]interface{}) (common.Datum, error) {
	switch datum := datum.(type) {
	case common.WrappedSyntax:
		return datum, nil
	case PatternVariableReference:
		return bindings[datum.PatternVariable].(common.Syntax).Datum(), nil
	case common.Pair:
		if first, ok := datum.First.(Subtemplate); ok {
			data, err := evaluateSubtemplate(first, bindings)
			if err != nil {
				return nil, err
			}
			rest, err := evaluateSyntaxTemplate(datum.Rest, bindings)
			if err != nil {
				return nil, err
			}
			result := rest
			for i := len(data) - 1; i >= 0; i-- {
				result = common.Pair{data[i], result}
			}
			return result, nil
		}
		first, err := evaluateSyntaxTemplate(datum.First, bindings)
		if err != nil {
			return nil, err
		}
		rest, err := evaluateSyntaxTemplate(datum.Rest, bindings)
		if err != nil {
			return nil, err
		}
		return common.Pair{first, rest}, nil
	default:
		return nil, fmt.Errorf("evaluate: unhandled syntax template %#v", datum)
	}
}

func evaluateSubtemplate(subtemplate Subtemplate, bindings map[*common.PatternVariable]interface{}) ([]common.Datum, error) {
	if subtemplate.Nesting == 0 {
		datum, err := evaluateSyntaxTemplate(subtemplate.Subtemplate.Template, bindings)
		if err != nil {
			return nil, err
		}
		return []common.Datum{datum}, nil
	}
	n := len(bindings[subtemplate.PatternVariables[0]].([]interface{}))
	for _, patternVariable := range subtemplate.PatternVariables {
		if len(bindings[patternVariable].([]interface{})) != n {
			return nil, fmt.Errorf("evaluate: differing number of matches for syntax template")
		}
	}
	var data []common.Datum
	for j := 0; j < n; j++ {
		nestedBindings := make(map[*common.PatternVariable]interface{}, len(subtemplate.Subtemplate.PatternVariables))
		for _, patternVariable := range subtemplate.Subtemplate.PatternVariables {
			nestedBindings[patternVariable] = bindings[patternVariable]
		}
		for _, patternVariable := range subtemplate.PatternVariables {
			nestedBindings[patternVariable] = nestedBindings[patternVariable].([]interface{})[j]
		}
		nestedData, err := evaluateSubtemplate(Subtemplate{subtemplate.Subtemplate, subtemplate.Nesting - 1, subtemplate.PatternVariables}, nestedBindings)
		if err != nil {
			return nil, err
		}
		data = append(data, nestedData...)
	}
	return data, nil
}

// Subtemplate represents the repitition of a SyntaxTemplate.
type Subtemplate struct {
	Subtemplate      SyntaxTemplate
	Nesting          int
	PatternVariables []*common.PatternVariable
}

// PatternVariableReference represents a reference to a pattern variable.
type PatternVariableReference struct {
	PatternVariable          *common.PatternVariable
	PatternVariableReference common.StackFrameReference
}

// Literal represents a literal value.

type Literal struct {
	Datum common.Datum
}

func (e Literal) Evaluate(c common.Continuation, s common.Stack) (common.Evaluation, error) {
	return common.CallC(c, e.Datum)
}
