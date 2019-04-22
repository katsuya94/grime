package common

import "fmt"

type Pattern interface {
	Match(Syntax) (map[*PatternVariable]interface{}, bool)
}

type patternLiteral struct {
	id Identifier
}

func (p patternLiteral) Match(syntax Syntax) (map[*PatternVariable]interface{}, bool) {
	if syntax, ok := syntax.Identifier(); ok {
		syntaxLocation := syntax.Location()
		patternLocation := p.id.Location()
		if (syntaxLocation == nil && patternLocation == nil && syntax.Equal(p.id)) ||
			(syntaxLocation != nil && patternLocation != nil && syntaxLocation == patternLocation) {
			return map[*PatternVariable]interface{}{}, true
		}
	}
	return nil, false
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
	subPattern  Pattern
	restPattern Pattern
}

func (p patternEllipsis) Match(syntax Syntax) (map[*PatternVariable]interface{}, bool) {
	var (
		result     map[*PatternVariable]interface{}
		subResults []map[*PatternVariable]interface{}
	)
	for {
		if pair, ok := syntax.Pair(); ok {
			first := NewSyntax(pair.First)
			rest := NewSyntax(pair.Rest)
			if subResult, ok := p.subPattern.Match(first); ok {
				subResults = append(subResults, subResult)
				restResult, ok := p.restPattern.Match(rest)
				if ok {
					_, ok := p.Match(rest)
					if !ok {
						// If the rest matches the restpattern, but does not match the ellipsis, we are done.
						result = restResult
						break
					}
				}
				// Otherwise continue.
				syntax = rest
				continue
			}
		}
		restResult, ok := p.restPattern.Match(syntax)
		if !ok {
			return nil, false
		}
		// If the input is not a pair and it matches restPattern, we are done.
		result = restResult
		break
	}
	// TODO: aggregate results
	return result, true
}

type patternPair struct {
	first Pattern
	rest  Pattern
}

type patternDatum struct {
	datum Datum
}

type PatternVariableInfo struct {
	Id              Identifier
	PatternVariable *PatternVariable
}

func CompilePattern(syntax Syntax) (Pattern, []PatternVariableInfo, error) {
	if syntax, ok := syntax.Identifier(); ok {
		location := syntax.Location()
		if location, ok := location.(*Literal); ok {
			return patternLiteral{location.Id}, nil, nil
		}
		if location == &UnderscoreKeyword {
			return patternUnderscore{}, nil, nil
		}
		if location == &EllipsisKeyword {
			return nil, nil, fmt.Errorf("pattern: invalid use of ellipsis")
		}
		variable := &PatternVariable{}
		return patternVariable{variable}, []PatternVariableInfo{{syntax, variable}}, nil
	}
	if syntax, ok := syntax.Pair(); ok {
		if cdr, ok := NewSyntax(syntax.Rest).Pair(); ok {
			if cadr, ok := NewSyntax(cdr.First).Identifier(); ok {
				if cadr.Location() == EllipsisKeyword {
					subPattern, subPatternVariableInfos, err := CompilePattern(NewSyntax(syntax.First))
					if err != nil {
						return nil, nil, err
					}
					for _, subPatternVariableInfo := range subPatternVariableInfos {
						subpatternVariableInfo.PatternVariable.Nesting++
					}
					cddr := NewSyntax(cdr.Rest)
					restPattern, restPatternVariableInfos, err := CompilePattern(cddr)
					if err != nil {
						return nil, nil, err
					}
					patternVariableInfos, err := mergePatternVariableInfos(subpatternVariableInfos, restPatternVariableInfos)
					if err != nil {
						return nil, nil, err
					}
					return patternEllipsis{subPattern, restPattern}, patternVariableInfos, nil
				}
			}
		}
		firstPattern, firstPatternVariableInfos, err := CompilePattern(NewSyntax(syntax.First))
		if err != nil {
			return nil, nil, err
		}
		restPattern, restPatternVariableInfos, err := CompilePattern(NewSyntax(syntax.Rest))
		if err != nil {
			return nil, nil, err
		}
		patternVariableInfos, err := mergePatternVariableInfos(firstPatternVariableInfos, restPatternVariableInfos)
		if err != nil {
			return nil, nil, err
		}
		return patternPair{firstPattern, restPattern}, append(firstPatternVariables, restPatternVariables...), nil
	}
	return patternDatum{syntax.Datum()}, nil, nil
}

func mergePatternVariableInfos(leftPatternVariableInfos, rightPatternVariableInfos []PatternVariableInfo) ([]PatternVariableInfo, error) {
	for _, leftPatternVariableInfo := range leftPatternVariableInfos {
		for _, rightPatternVariableInfo := range rightPatternVariableInfos {
			if leftPatternVariableInfo.Id.Equal(rightPatternVariableInfo.Id) {
				return nil, fmt.Errorf("pattern: duplicate pattern variable %v", leftPatternVariableInfo.Id.Name())
			}
		}
	}
	return append(leftPatternVariableInfos, rightPatternVariableInfos), nil
}
