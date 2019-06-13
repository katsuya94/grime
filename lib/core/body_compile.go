package core

import (
	"fmt"

	"github.com/katsuya94/grime/common"
)

func BodyCompile(compiler Compiler, forms []common.Syntax, scope common.Scope, frameTemplate *common.FrameTemplate, stack common.Stack) (common.Expression, error) {
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
				// TODO: the bindings for the next phase are useful for evaluating the RHS, but not necessarily the output of the transformer
				// "If a macro transformer inserts a free reference to an identifier, the reference refers to the binding that was visible where the transformer was specified, regardless of any local bindings that may surround the use of the macro."
				// "Each time the expander encounters a binding form it creates a set of substitutions, each mapping one of the (possibly marked) bound identifiers to information about the binding. (For a lambda expression, the expander might map each bound identifier to a representation of the formal parameter in the output of the expander. For a let-syntax form, the expander might map each bound identifier to the associated transformer.) These substitutions are applied to the portions of the input form in which the binding is supposed to be visible."
				// "When a substitution is created to map an identifier to an expand-time value, the substitution records the name of the identifier and the set of marks that have been applied to that identifier, along with the associated expand-time value. The expander resolves identifier references by looking for the latest matching substitution to be applied to the identifier, i.e., the outermost substitution in the wrap whose name and marks match the name and marks recorded in the substitution. The name matches if it is the same name (if using symbols, then by eq?), and the marks match if the marks recorded with the substitution are the same as those that appear below the substitution in the wrap, i.e., those that were applied before the substitution. Marks applied after a substitution, i.e., appear over the substitution in the wrap, are not relevant and are ignored."
				// What does ^ mean?
				// From Racket's syntax model: When a form parses as the binding of a particular identifier, parsing updates a global table that maps a combination of an identifier’s symbol and scope set to its meaning: a variable, a syntactic form, or a transformer. An identifier refers to a particular binding when the reference’s symbol and the identifier’s symbol are the same, and when the reference’s scope set is a superset of the binding’s scope set. For a given identifier, multiple bindings may have scope sets that are subsets of the identifier’s; in that case, the identifier refers to the binding whose set is a superset of all others; if no such binding exists, the reference is ambiguous (and triggers a syntax error if it is parsed as an expression). A binding shadows any binding (i.e., it is shadowing any binding) that the same symbol but a subset of scopes.
				// A reference to a local binding in a fully expanded program has a scope set that matches its binding identifier exactly. Additional scopes, if any, are removed. As a result, bound-identifier=? can be used to correlate local binding identifiers with reference identifiers, while free-identifier=? must be used to relate references to module bindings or top-level bindings.
				expression, err := compiler.ExpressionCompile(form, frameTemplate, stack)
				if err != nil {
					return nil, ExpressionCompileError{err, "right-hand side of syntax definition", sourceLocations[i]}
				}
				value, err := common.Evaluate(stack, expression)
				if err != nil {
					return nil, err
				}
				procedure, ok := value.(common.Procedure)
				if !ok {
					return nil, fmt.Errorf("compile: define-syntax: expected procedure")
				}
				stack.Set(keyword.TransformerReference(common.CurrentStackContext), procedure)
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
				expanded, ok, err := compiler.Expand(common.NewSyntax(v), stack)
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
		expression, err := compiler.ExpressionCompile(definitionForms[i], frameTemplate, stack)
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
		expression, err := compiler.ExpressionCompile(forms[j], frameTemplate, stack)
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
