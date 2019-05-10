package common

import "fmt"

var (
	UnderscoreKeyword = &Keyword{Function(func(Continuation, ...Datum) (Evaluation, error) {
		return nil, fmt.Errorf("cannot expand underscore")
	})}
	EllipsisKeyword = &Keyword{Function(func(Continuation, ...Datum) (Evaluation, error) {
		return nil, fmt.Errorf("cannot expand ellipsis")
	})}
)

type Pattern interface {
	Match(Syntax) (map[*PatternVariable]interface{}, bool)
}

type patternLiteral struct {
	id Identifier
}

func (p patternLiteral) Match(syntax Syntax) (map[*PatternVariable]interface{}, bool) {
	id, ok := syntax.Identifier()
	if !ok {
		return nil, false
	}
	if !id.FreeEqual(p.id) {
		return nil, false
	}
	return map[*PatternVariable]interface{}{}, true
}

type patternUnderscore struct{}

func (p patternUnderscore) Match(syntax Syntax) (map[*PatternVariable]interface{}, bool) {
	return map[*PatternVariable]interface{}{}, true
}

type patternVariable struct {
	patternVariable *PatternVariable
}

func (p patternVariable) Match(syntax Syntax) (map[*PatternVariable]interface{}, bool) {
	return map[*PatternVariable]interface{}{p.patternVariable: syntax}, true
}

type patternEllipsis struct {
	subPattern          Pattern
	subPatternVariables []*PatternVariable
	restPattern         Pattern
}

func (p patternEllipsis) Match(syntax Syntax) (map[*PatternVariable]interface{}, bool) {
	var (
		result     map[*PatternVariable]interface{}
		subResults []map[*PatternVariable]interface{}
	)
	for {
		if pair, ok := syntax.Pair(); ok {
			// If the syntax is a pair...
			first := NewSyntax(pair.First)
			rest := NewSyntax(pair.Rest)
			if subResult, ok := p.subPattern.Match(first); ok {
				// And its first matches subPattern...
				subResults = append(subResults, subResult)
				if restResult, ok := p.restPattern.Match(rest); ok {
					// And its rest matches restPattern...
					if _, ok := p.Match(rest); !ok {
						// But its rest does not match the ellipsis, we are done.
						result = restResult
						break
					}
				}
				// And the rest doesn't need to match restPattern, continue.
				syntax = rest
				continue
			}
		}
		// If the syntax is not a pair or its first doesn't match subPattern...
		if restResult, ok := p.restPattern.Match(syntax); ok {
			// And the syntax matches restPattern, we are done.
			result = restResult
			break
		}
		// And the syntax does not match restPattern, fail.
		return nil, false
	}
	for _, patternVariable := range p.subPatternVariables {
		result[patternVariable] = []interface{}{}
	}
	for _, subResult := range subResults {
		for _, patternVariable := range p.subPatternVariables {
			result[patternVariable] = append(result[patternVariable].([]interface{}), subResult[patternVariable])
		}
	}
	return result, true
}

type patternPair struct {
	first Pattern
	rest  Pattern
}

func (p patternPair) Match(syntax Syntax) (map[*PatternVariable]interface{}, bool) {
	pair, ok := syntax.Pair()
	if !ok {
		return nil, false
	}
	firstResult, ok := p.first.Match(NewSyntax(pair.First))
	if !ok {
		return nil, false
	}
	restResult, ok := p.rest.Match(NewSyntax(pair.Rest))
	if !ok {
		return nil, false
	}
	result := map[*PatternVariable]interface{}{}
	for patternVariable, match := range firstResult {
		result[patternVariable] = match
	}
	for patternVariable, match := range restResult {
		result[patternVariable] = match
	}
	return result, true
}

type patternDatum struct {
	datum Datum
}

func (p patternDatum) Match(syntax Syntax) (map[*PatternVariable]interface{}, bool) {
	if syntax.Unwrap() != p.datum {
		return nil, false
	}
	return map[*PatternVariable]interface{}{}, true
}

type PatternVariableInfo struct {
	Id              Identifier
	PatternVariable *PatternVariable
}

func CompilePattern(syntax Syntax, frameTemplate *FrameTemplate) (Pattern, []PatternVariableInfo, error) {
	if syntax, ok := syntax.Identifier(); ok {
		location := syntax.Location()
		if location, ok := location.(*Literal); ok {
			return patternLiteral{location.Id}, nil, nil
		}
		if location == UnderscoreKeyword {
			return patternUnderscore{}, nil, nil
		}
		if location == EllipsisKeyword {
			return nil, nil, fmt.Errorf("pattern: invalid use of ellipsis")
		}
		variable := &PatternVariable{}
		frameTemplate.Add()
		return patternVariable{variable}, []PatternVariableInfo{{syntax, variable}}, nil
	}
	if syntax, ok := syntax.Pair(); ok {
		if cdr, ok := NewSyntax(syntax.Rest).Pair(); ok {
			if cadr, ok := NewSyntax(cdr.First).Identifier(); ok {
				if cadr.Location() == EllipsisKeyword {
					subPattern, subPatternVariableInfos, err := CompilePattern(NewSyntax(syntax.First), frameTemplate)
					if err != nil {
						return nil, nil, err
					}
					subPatternVariables := []*PatternVariable{}
					for _, subPatternVariableInfo := range subPatternVariableInfos {
						subPatternVariableInfo.PatternVariable.Nesting++
						subPatternVariables = append(subPatternVariables, subPatternVariableInfo.PatternVariable)
					}
					cddr := NewSyntax(cdr.Rest)
					restPattern, restPatternVariableInfos, err := CompilePattern(cddr, frameTemplate)
					if err != nil {
						return nil, nil, err
					}
					patternVariableInfos := append(subPatternVariableInfos, restPatternVariableInfos...)
					return patternEllipsis{subPattern, subPatternVariables, restPattern}, patternVariableInfos, nil
				}
			}
		}
		firstPattern, firstPatternVariableInfos, err := CompilePattern(NewSyntax(syntax.First), frameTemplate)
		if err != nil {
			return nil, nil, err
		}
		restPattern, restPatternVariableInfos, err := CompilePattern(NewSyntax(syntax.Rest), frameTemplate)
		if err != nil {
			return nil, nil, err
		}
		patternVariableInfos := append(firstPatternVariableInfos, restPatternVariableInfos...)
		return patternPair{firstPattern, restPattern}, patternVariableInfos, nil
	}
	return patternDatum{syntax.Unwrap()}, nil, nil
}
