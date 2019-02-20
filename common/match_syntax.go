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

func Pattern(d Datum) Syntax {
	syntax := NewSyntax(NewWrappedSyntax(d, nil))
	syntax.Push(patternScope, LEXICAL)
	return syntax
}

type MatchResult map[Identifier][]Syntax

func NewEmptyMatchResult() MatchResult {
	return MatchResult{}
}

func NewSingleMatchResult(id Identifier, syntax Syntax) MatchResult {
	return MatchResult{id: {syntax}}
}

func (mr MatchResult) Merge(other MatchResult) MatchResult {
	merged := NewEmptyMatchResult()
	// TODO
}

type syntaxMatcher struct {
	literals []Identifier
}

func (m *syntaxMatcher) match(input Syntax, pattern Syntax) (MatchResult, bool, error) {
	if pattern, ok := pattern.Identifier(); ok {
		if id, ok := input.Identifier(); ok {
			location := id.Location()
			for _, literal := range m.literals {
				l := literal.Location()
				if (location == nil && l == nil && pattern.Equal(id)) || (location != nil && l != nil && location == l) {
					return NewEmptyMatchResult(), true, nil
				}
			}
		}
		return NewSingleMatchResult(pattern, input), true, nil
	}
	if pattern, ok := pattern.Pair(); ok {
		if rest, ok := NewSyntax(pattern.Rest).Pair(); ok {
			if rest.First == Ellipsis {
				return m.matchEllipsis(input, NewSyntax(pattern.First), NewSyntax(rest.Rest))
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
		return firstResult.Merge(restResult), true, nil
	}
	switch p := pattern.(type) {
	case Boolean, Number, Character, String:
		if input.Unwrap() == p {
			return map[Symbol]interface{}{}, true, nil
		}
		return nil, false, nil
	case Pair:
		if rest, ok := p.Rest.(Pair); ok {
			if rest.First == Ellipsis {
				return m.matchEllipsis(input, p.First, rest.Rest)
			}
		}
		pair, ok := input.Pair()
		if !ok {
			return nil, false, nil
		}
		first := Syntax{pair.First}
		rest := Syntax{pair.Rest}
		firstResult, match, err := m.match(first, p.First)
		if err != nil {
			return nil, false, err
		} else if !match {
			return nil, false, nil
		}
		restResult, match, err := m.match(rest, p.Rest)
		if err != nil {
			return nil, false, err
		} else if !match {
			return nil, false, nil
		}
		result := map[Symbol]interface{}{}
		for k, v := range firstResult {
			result[k] = v
		}
		for k, v := range restResult {
			result[k] = v
		}
		return result, true, nil
	default:
		if p == Null {
			if input.Unwrap() == Null {
				return map[Symbol]interface{}{}, true, nil
			}
			return nil, false, nil
		} else if p == Underscore {
			return map[Symbol]interface{}{}, true, nil
		}
		return nil, false, fmt.Errorf("match: unhandled pattern %#v", p)
	}
}

func (m *syntaxMatcher) matchEllipsis(input Syntax, subpattern Syntax, restpattern Syntax) (MatchResult, bool, error) {
	var (
		result     map[Symbol]interface{}
		subresults []map[Symbol]interface{}
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
		first := Syntax{pair.First}
		rest := Syntax{pair.Rest}
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
	for name, _ := range patternVariables {
		result[name] = []interface{}{}
	}
	for _, subresult := range subresults {
		for k, v := range subresult {
			result[k] = append(result[k].([]interface{}), v)
		}
	}
	return result, true, nil
}

func PatternVariables(pattern Syntax, literals map[Symbol]Location) (map[Symbol]int, error) {
	switch p := pattern.(type) {
	case Boolean, Number, Character, String:
		return nil, nil
	case Symbol:
		if _, ok := literals[p]; ok {
			return nil, nil
		} else {
			return map[Symbol]int{p: 0}, nil
		}
	case Pair:
		if rest, ok := p.Rest.(Pair); ok {
			if rest.First == Ellipsis {
				subpatternVariables, err := PatternVariables(p.First, literals)
				if err != nil {
					return nil, err
				}
				patternVariables := make(map[Symbol]int, len(subpatternVariables))
				for name, n := range subpatternVariables {
					patternVariables[name] = n + 1
				}
				return patternVariables, nil
			}
		}
		firstPatternVariables, err := PatternVariables(p.First, literals)
		if err != nil {
			return nil, err
		}
		restPatternVariables, err := PatternVariables(p.Rest, literals)
		if err != nil {
			return nil, err
		}
		patternVariables := make(map[Symbol]int, len(firstPatternVariables)+len(restPatternVariables))
		for name, n := range firstPatternVariables {
			patternVariables[name] = n
		}
		for name, n := range restPatternVariables {
			if _, ok := patternVariables[name]; ok {
				return nil, fmt.Errorf("match: duplicate pattern variable %v", name)
			}
			patternVariables[name] = n
		}
		return patternVariables, nil
	default:
		if p == Null || p == Underscore || p == Ellipsis {
			return nil, nil
		}
		return nil, fmt.Errorf("match: unhandled pattern %#v", p)
	}
}

func MatchSyntax(input Syntax, pattern Datum, literals []Identifier) (map[Symbol]interface{}, bool, error) {
	return &syntaxMatcher{literals}.match(input, pattern)
}
