package common

var simplePatternScope BaseScope

func init() {
	simplePatternScope = NewScope()
	simplePatternScope.Set(NewIdentifier(Symbol("_")), UnderscoreKeyword)
	simplePatternScope.Set(NewIdentifier(Symbol("...")), EllipsisKeyword)
}

type SimplePattern struct {
	pattern              Pattern
	patternVariableInfos []PatternVariableInfo
}

func MustCompileSimplePattern(datum Datum, literals ...Symbol) SimplePattern {
	ids := []Identifier{}
	for _, literal := range literals {
		ids = append(ids, NewIdentifier(literal))
	}
	return MustCompileSimplePatternWithIdentifierLiterals(datum, ids...)
}

func MustCompileSimplePatternWithIdentifierLiterals(datum Datum, literals ...Identifier) SimplePattern {
	syntax := NewSyntax(NewWrappedSyntax(datum, nil))
	syntax = syntax.Push(simplePatternScope, LEXICAL)
	if len(literals) > 0 {
		literalScope := NewScope()
		for _, literal := range literals {
			// when compiling, treat an identifier with the same name as a literal for the provided identifier
			id := NewIdentifier(literal.Name())
			literalScope.Set(id, &Literal{literal})
		}
		syntax = syntax.Push(literalScope, LEXICAL)
	}
	pattern, patternVariableInfos, err := CompilePattern(syntax)
	if err != nil {
		panic(err)
	}
	return SimplePattern{pattern, patternVariableInfos}
}

func (sp SimplePattern) Match(syntax Syntax) (map[Symbol]interface{}, bool) {
	result, ok := sp.pattern.Match(syntax)
	if !ok {
		return nil, false
	}
	simpleResult := map[Symbol]interface{}{}
	for _, patternVariableInfo := range sp.patternVariableInfos {
		match := result[patternVariableInfo.PatternVariable]
		simpleResult[patternVariableInfo.Id.Name()] = match
	}
	return simpleResult, true
}
