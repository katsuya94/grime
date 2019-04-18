package common

import "fmt"

type Pattern interface {
	Match(Syntax) (map[*PatternVariable]interface{}, bool)
}

type patternDatum struct {
	datum Datum
}

type patternLiteral struct {
	id Identifier
}

type patternUnderscore struct{}

type patternVariable struct {
	patternVariable *PatternVariable
}

type patternEllipsis struct {
	pattern Pattern
}

type patternPair struct {
	first Pattern
	rest  Pattern
}

type PatternVariableInfo struct {
	PatternVariable *PatternVariable
	Nesting         int
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
		// make patten variable
	}
	if pattern, ok := pattern.Pair(); ok {
		if rest, ok := NewSyntax(pattern.Rest).Pair(); ok {
			if id, ok := NewSyntax(rest.First).Identifier(); ok {
				if id.Location() == EllipsisKeyword {
					subpatternVariables, err := PatternVariables(NewSyntax(pattern.First), literals)
					if err != nil {
						return nil, err
					}
					patternVariables := make(MatchInfoSet, len(subpatternVariables))
					for i := range subpatternVariables {
						patternVariables[i] = MatchInfo{
							subpatternVariables[i].Id,
							subpatternVariables[i].Nesting + 1,
						}
					}
					return patternVariables, nil
				}
			}
		}
		firstPatternVariables, err := PatternVariables(NewSyntax(pattern.First), literals)
		if err != nil {
			return nil, err
		}
		restPatternVariables, err := PatternVariables(NewSyntax(pattern.Rest), literals)
		if err != nil {
			return nil, err
		}

		for _, firstPatternVariable := range firstPatternVariables {
			for _, restPatternVariable := range restPatternVariables {
				if firstPatternVariable.Id.Equal(restPatternVariable.Id) {
					return nil, fmt.Errorf("match: duplicate pattern variable %v", firstPatternVariable.Id.Name())
				}
			}
		}
		return append(firstPatternVariables, restPatternVariables...), nil
	}
	return nil, nil
}
