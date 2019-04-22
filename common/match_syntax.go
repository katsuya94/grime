package common

import (
	"fmt"
)

var (
	UnderscoreKeyword = Keyword{Function(func(Continuation, ...Datum) (EvaluationResult, error) {
		return nil, fmt.Errorf("cannot expand underscore")
	})}
	EllipsisKeyword = Keyword{Function(func(Continuation, ...Datum) (EvaluationResult, error) {
		return nil, fmt.Errorf("cannot expand ellipsis")
	})}
)

var patternScope BaseScope

func init() {
	patternScope = NewScope()
	patternScope.Set(NewIdentifier(Symbol("_")), UnderscoreKeyword)
	patternScope.Set(NewIdentifier(Symbol("...")), EllipsisKeyword)
}

// func Pattern(d Datum) Syntax {
// 	syntax := NewSyntax(NewWrappedSyntax(d, nil))
// 	syntax.Push(patternScope, LEXICAL)
// 	return syntax
// }

type MatchResult struct {
	Id    Identifier
	Match interface{}
}

type MatchResultSet []MatchResult

func (mrs MatchResultSet) push(other MatchResultSet) {
	for _, other := range other {
		found := false
		for i := range mrs {
			if other.Id.Equal(mrs[i].Id) {
				mrs[i].Match = append(mrs[i].Match.([]interface{}), other.Match)
				found = true
				break
			}
		}
		if !found {
			panic("could not resolve pattern variable in match result set")
		}
	}
}

func (mrs MatchResultSet) Get(id Identifier) interface{} {
	for _, mr := range mrs {
		if id.Equal(mr.Id) {
			return mr.Match
		}
	}
	return nil
}

type MatchInfo struct {
	Id      Identifier
	Nesting int
}

type MatchInfoSet []MatchInfo

type syntaxMatcher struct {
	literals []Identifier
}

func (m syntaxMatcher) match(input Syntax, pattern Syntax) (MatchResultSet, bool, error) {
	if pattern, ok := pattern.Identifier(); ok {
		if id, ok := input.Identifier(); ok {
			location := id.Location()
			for _, literal := range m.literals {
				if pattern.Equal(literal) {
					l := literal.Location()
					match := (location == nil && l == nil && pattern.Equal(id)) || (location != nil && l != nil && location == l)
					return nil, match, nil
				}
			}
		}
		if pattern.Location() == &UnderscoreKeyword {
			return nil, true, nil
		}
		return MatchResultSet{{pattern, input}}, true, nil
	}
	if pattern, ok := pattern.Pair(); ok {
		if rest, ok := NewSyntax(pattern.Rest).Pair(); ok {
			if id, ok := NewSyntax(rest.First).Identifier(); ok {
				if id.Location() == EllipsisKeyword {
					return m.matchEllipsis(input, NewSyntax(pattern.First), NewSyntax(rest.Rest))
				}
			}
		}
		input, ok := input.Pair()
		if !ok {
			return nil, false, nil
		}
		firstResult, match, err := m.match(NewSyntax(input.First), NewSyntax(pattern.First))
		if err != nil {
			return nil, false, err
		} else if !match {
			return nil, false, nil
		}
		restResult, match, err := m.match(NewSyntax(input.Rest), NewSyntax(pattern.Rest))
		if err != nil {
			return nil, false, err
		} else if !match {
			return nil, false, nil
		}
		return append(firstResult, restResult...), true, nil
	}
	if input.Unwrap() == pattern.Unwrap() {
		return nil, true, nil
	}
	return nil, false, nil
}

func (m syntaxMatcher) matchEllipsis(input Syntax, subpattern Syntax, restpattern Syntax) (MatchResultSet, bool, error) {
	var (
		result     MatchResultSet
		subresults []MatchResultSet
	)
	for {
		pair, ok := input.Pair()
		if !ok {
			restResult, ok, err := m.match(input, restpattern)
			if err != nil {
				return nil, false, err
			} else if !ok {
				return nil, false, nil
			}
			// If the input is not a pair and it matches restpattern, we are done.
			result = restResult
			break
		}
		first := NewSyntax(pair.First)
		rest := NewSyntax(pair.Rest)
		subresult, ok, err := m.match(first, subpattern)
		if err != nil {
			return nil, false, err
		} else if !ok {
			restResult, ok, err := m.match(input, restpattern)
			if err != nil {
				return nil, false, err
			} else if !ok {
				return nil, false, nil
			}
			// If the first does not match subpattern, but the pair matches restpattern, we are done.
			result = restResult
			break
		}
		subresults = append(subresults, subresult)
		restResult, ok, err := m.match(rest, restpattern)
		if err != nil {
			return nil, false, nil
		} else if ok {
			_, ok, err := m.matchEllipsis(rest, subpattern, restpattern)
			if err != nil {
				return nil, false, err
			} else if !ok {
				// If the rest matches the restpattern, but does not match the ellipsis, we are done.
				result = restResult
				break
			}
		}
		// Otherwise continue.
		input = rest
	}
	patternVariables, err := PatternVariables(subpattern, m.literals)
	if err != nil {
		return nil, false, err
	}
	for _, patternVariable := range patternVariables {
		result = append(result, MatchResult{patternVariable.Id, []interface{}{}})
	}
	for _, subresult := range subresults {
		result.push(subresult)
	}
	return result, true, nil
}

func PatternVariables(pattern Syntax, literals []Identifier) (MatchInfoSet, error) {
	if pattern, ok := pattern.Identifier(); ok {
		for _, literal := range literals {
			if pattern.Equal(literal) {
				return nil, nil
			}
		}
		return MatchInfoSet{{pattern, 0}}, nil
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

func MatchSyntax(input Syntax, pattern Syntax, literals []Identifier) (MatchResultSet, bool, error) {
	return syntaxMatcher{literals}.match(input, pattern)
}

type MatchResultSetSimple struct {
	MatchResultSet
}

func (mrss MatchResultSetSimple) Get(idStr string) interface{} {
	return mrss.MatchResultSet.Get(NewIdentifier(Symbol(idStr)))
}

func MatchSyntaxSimple(input Syntax, pattern Syntax, literalStrs ...string) (MatchResultSetSimple, bool, error) {
	literals := make([]Identifier, len(literalStrs))
	for i, literalStr := range literalStrs {
		literals[i] = NewIdentifier(Symbol(literalStr))
	}
	result, ok, err := MatchSyntax(input, pattern, literals)
	return MatchResultSetSimple{result}, ok, err
}
