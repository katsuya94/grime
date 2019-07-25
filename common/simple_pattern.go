package common

var simplePatternScope = NewScope()
var simplePatternEnvironment = NewEnvironment()

func init() {
	underscoreId := NewSyntax(NewIdentifier(Symbol("_")).Push(simplePatternScope, 0)).IdentifierOrDie()
	underscoreBinding := NewBinding()
	simplePatternScope.Add(underscoreId, underscoreBinding)
	simplePatternEnvironment.Add(underscoreBinding, NewSyntacticAbstraction(UnderscoreTransformer))
	ellipsisId := NewSyntax(NewIdentifier(Symbol("...")).Push(simplePatternScope, 0)).IdentifierOrDie()
	ellipsisBinding := NewBinding()
	simplePatternScope.Add(ellipsisId, ellipsisBinding)
	simplePatternEnvironment.Add(ellipsisBinding, NewSyntacticAbstraction(EllipsisTransformer))
}

type SimplePattern struct {
	pattern              Pattern
	patternVariableInfos []PatternVariableInfo
}

func CompileSimplePattern(datum Datum, literals ...Symbol) (SimplePattern, error) {
	ids := []Identifier{}
	for _, literal := range literals {
		ids = append(ids, NewIdentifier(literal))
	}
	return CompileSimplePatternWithIdentifierLiterals(datum, ids...)
}

func MustCompileSimplePattern(datum Datum, literals ...Symbol) SimplePattern {
	simplePattern, err := CompileSimplePattern(datum, literals...)
	if err != nil {
		panic(err)
	}
	return simplePattern
}

func CompileSimplePatternWithIdentifierLiterals(datum Datum, literals ...Identifier) (SimplePattern, error) {
	syntax := NewSyntax(NewWrappedSyntax(datum, nil))
	syntax = syntax.Push(simplePatternScope, 0)
	env := simplePatternEnvironment
	if len(literals) > 0 {
		literalScope := NewScope()
		for _, literal := range literals {
			// when compiling, treat an identifier with the same name as a literal for the provided identifier
			id := NewIdentifier(literal.Name())
			binding := NewBinding()
			literalScope.Add(id, binding)
			(&env).Extend(binding, NewPatternLiteral(literal))
		}
		syntax = syntax.Push(literalScope, 0)
	}
	pattern, patternVariableInfos, err := CompilePattern(syntax, env)
	if err != nil {
		return SimplePattern{}, err
	}
	return SimplePattern{pattern, patternVariableInfos}, nil
}

func MustCompileSimplePatternWithIdentifierLiterals(datum Datum, literals ...Identifier) SimplePattern {
	simplePattern, err := CompileSimplePatternWithIdentifierLiterals(datum, literals...)
	if err != nil {
		panic(err)
	}
	return simplePattern
}

func (sp SimplePattern) Match(syntax Syntax) (map[Symbol]interface{}, bool) {
	result, ok := sp.pattern.Match(syntax)
	if !ok {
		return nil, false
	}
	simpleResult := map[Symbol]interface{}{}
	for _, patternVariableInfo := range sp.patternVariableInfos {
		match := result[patternVariableInfo.Binding]
		simpleResult[patternVariableInfo.Id.Name()] = match
	}
	return simpleResult, true
}
