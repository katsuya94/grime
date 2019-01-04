package core

import (
	"fmt"

	"github.com/katsuya94/grime/common"
)

func BodyCompile(compiler Compiler, forms []common.Datum, scope *common.Scope) (common.Expression, error) {
	var (
		i                         int
		sourceLocations           []common.SourceLocation
		definitionVariables       []*common.Variable
		definitionForms           []common.Datum
		definitionSourceLocations []common.SourceLocation
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
			switch v := form.(type) {
			case DefineSyntaxForm:
				form := v.Form
				keyword := &common.Keyword{}
				err := scope.Set(v.Identifier, keyword)
				if err != nil {
					return nil, err
				}
				expression, err := compiler.Next().ExpressionCompile(form)
				if err != nil {
					return nil, ExpressionCompileError{err, "right-hand side of syntax definition", sourceLocations[i]}
				}
				value, err := common.EvaluateOnce(expression)
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
				variable := &common.Variable{}
				err := scope.Set(v.Identifier, variable)
				if err != nil {
					return nil, err
				}
				definitionVariables = append(definitionVariables, variable)
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
				expanded, ok, err := compiler.Expand(v)
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
		expression, err := compiler.ExpressionCompile(definitionForms[i])
		if err != nil {
			return nil, ExpressionCompileError{err, "right-hand side of definition", definitionSourceLocations[i]}
		}
		expressions = append(expressions, Define{definitionVariables[i], expression})
	}
	// Compile the remaining expressions.
	if len(forms[i:]) == 0 {
		return nil, common.ErrUnexpectedFinalForm
	}
	for j := i; j < len(forms); j++ {
		expression, err := compiler.ExpressionCompile(forms[j])
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

func sourceLocation(form common.Datum) common.SourceLocation {
	if form, ok := form.(common.WrappedSyntax); ok {
		return form.SourceLocation()
	}
	return common.SourceLocation{}
}
