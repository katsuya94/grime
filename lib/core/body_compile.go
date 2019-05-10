package core

import (
	"fmt"

	"github.com/katsuya94/grime/common"
)

func BodyCompile(compiler Compiler, forms []common.Syntax, scope common.Scope, frameTemplate *common.FrameTemplate) (common.Expression, error) {
	var (
		i                            int
		sourceLocations              []common.SourceLocation
		definitionVariables          []*common.Variable
		definitionVariableReferences []common.StackFrameReference
		definitionForms              []common.Syntax
		definitionSourceLocations    []common.SourceLocation
	)
	for _, form := range forms {
		sourceLocations = append(sourceLocations, sourceLocation(form))
	}
	// Expand and handle definitions, deferring expansion of variable definitions.
	for i = 0; i < len(forms); i++ {
		processed := false
		expression := false
		for !processed {
			form := forms[i]
			switch v := form.Datum().(type) {
			case DefineSyntaxForm:
				keyword := common.NewKeyword(frameTemplate)
				err := scope.Set(v.Identifier, keyword)
				if err != nil {
					return nil, err
				}
				rhsScope := common.NewScope()
				err = rhsScope.Set(v.Identifier, keyword)
				if err != nil {
					return nil, err
				}
				form := v.Form.Push(rhsScope, common.LEXICAL, false).Next()
				expression, err := compiler.ExpressionCompile(form, frameTemplate)
				if err != nil {
					return nil, ExpressionCompileError{err, "right-hand side of syntax definition", sourceLocations[i]}
				}
				// TODO: where does the stack come from?
				value, err := common.Evaluate(common.NewStack(nil), expression)
				if err != nil {
					return nil, err
				}
				procedure, ok := value.(common.Procedure)
				if !ok {
					return nil, fmt.Errorf("compile: define-syntax: expected procedure")
				}
				keyword.Transformer = procedure
				processed = true
			case DefineForm:
				form := v.Form
				variable := common.NewVariable(frameTemplate)
				err := scope.Set(v.Identifier, variable)
				if err != nil {
					return nil, err
				}
				variableReference := variable.ValueReference(common.CurrentStackContext)
				definitionVariables = append(definitionVariables, variable)
				definitionVariableReferences = append(definitionVariableReferences, variableReference)
				definitionForms = append(definitionForms, form)
				definitionSourceLocations = append(definitionSourceLocations, sourceLocations[i])
				processed = true
			case BeginForm:
				if len(forms) == i+1 {
					processed = true
					expression = true
					break
				}
				beginSourceLocation := sourceLocations[i]
				following := forms[i+1:]
				followingSourceLocations := sourceLocations[i+1:]
				forms = forms[0:i]
				sourceLocations = sourceLocations[0:i]
				forms = append(forms, v.Forms...)
				for _, form := range v.Forms {
					sl := sourceLocation(form)
					if (sl == common.SourceLocation{}) {
						sl = beginSourceLocation
					}
					sourceLocations = append(sourceLocations, sl)
				}
				forms = append(forms, following...)
				sourceLocations = append(sourceLocations, followingSourceLocations...)
				i--
				processed = true
			case LetSyntaxForm:
				return nil, fmt.Errorf("compile: let-syntax not implemented")
			default:
				expanded, ok, err := compiler.Expand(common.NewSyntax(v))
				if err != nil {
					return nil, err
				} else if !ok {
					processed = true
					expression = true
					break
				}
				forms[i] = expanded
			}
		}
		if expression {
			break
		}
	}
	// Compile define expressions for the definitions.
	var expressions []common.Expression
	for i := range definitionVariables {
		expression, err := compiler.ExpressionCompile(definitionForms[i], frameTemplate)
		if err != nil {
			return nil, ExpressionCompileError{err, "right-hand side of definition", definitionSourceLocations[i]}
		}
		expressions = append(expressions, Define{definitionVariables[i], definitionVariableReferences[i], expression})
	}
	// Compile the remaining expressions.
	if len(forms[i:]) == 0 {
		return nil, common.ErrUnexpectedFinalForm
	}
	for j := i; j < len(forms); j++ {
		expression, err := compiler.ExpressionCompile(forms[j], frameTemplate)
		if err != nil {
			return nil, ExpressionCompileError{err, "body expression", sourceLocations[j]}
		}
		expressions = append(expressions, expression)
	}
	var expression common.Expression
	if len(expressions) > 1 {
		expression = Begin{expressions}
	} else {
		expression = expressions[0]
	}
	return expression, nil
}

func sourceLocation(form common.Syntax) common.SourceLocation {
	if form, ok := form.Datum().(common.WrappedSyntax); ok {
		return form.SourceLocation()
	}
	return common.SourceLocation{}
}
