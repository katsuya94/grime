package common

import (
	"fmt"
)

func Pattern(d Datum) Datum {
	if d, ok := d.(Pair); ok {
		return Pair{
			Pattern(d.First),
			Pattern(d.Rest),
		}
	}
	if d == Symbol("_") {
		return Underscore
	}
	if d == Symbol("...") {
		return Ellipsis
	}
	return d
}

type syntaxMatcher struct {
	literals map[Symbol]Location
}

func newSyntaxMatcher(literals map[Symbol]Location) *syntaxMatcher {
	return &syntaxMatcher{literals}
}

func (m *syntaxMatcher) match(input Datum, pattern Datum) (map[Symbol]interface{}, bool, error) {
	syntax, isSyntax := input.(WrappedSyntax)
	switch p := pattern.(type) {
	case Boolean, Number, Character, String, nil:
		if (isSyntax && syntax.Datum() == p) || input == p {
			return map[Symbol]interface{}{}, true, nil
		}
		return nil, false, nil
	case Symbol:
		if location, ok := m.literals[p]; ok {
			if !isSyntax {
				return nil, false, nil
			}
			name, l, ok := syntax.Identifier()
			if !ok {
				return nil, false, nil
			} else if location == nil && name == p {
				return map[Symbol]interface{}{}, true, nil
			} else if location != nil && l == location {
				return map[Symbol]interface{}{}, true, nil
			}
			return nil, false, nil
		}
		return map[Symbol]interface{}{p: input}, true, nil
	case Pair:
		if rest, ok := p.Rest.(Pair); ok {
			if rest.First == Ellipsis {
				return m.matchEllipsis(input, p.First, rest.Rest)
			}
		}
		var (
			first Datum
			rest  Datum
		)
		if isSyntax {
			pair, ok := syntax.Datum().(Pair)
			if !ok {
				return nil, false, nil
			}
			first = syntax.PushOnto(pair.First)
			rest = syntax.PushOnto(pair.Rest)
		} else {
			pair, ok := input.(Pair)
			if !ok {
				return nil, false, nil
			}
			first = pair.First
			rest = pair.Rest
		}
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
		if p == Underscore {
			return map[Symbol]interface{}{}, true, nil
		}
		return nil, false, fmt.Errorf("match: unhandled pattern %#v", p)
	}
}

func (m *syntaxMatcher) matchEllipsis(input Datum, subpattern Datum, restpattern Datum) (map[Symbol]interface{}, bool, error) {
	var (
		result     map[Symbol]interface{}
		subresults []map[Symbol]interface{}
	)
	for {
		syntax, isSyntax := input.(WrappedSyntax)
		var (
			pair   Pair
			isPair bool
			first  Datum
			rest   Datum
		)
		if isSyntax {
			pair, isPair = syntax.Datum().(Pair)
			if isPair {
				first = syntax.PushOnto(pair.First)
				rest = syntax.PushOnto(pair.Rest)
			}
		} else {
			pair, isPair = input.(Pair)
			if isPair {
				first = pair.First
				rest = pair.Rest
			}
		}
		if !isPair {
			restResult, ok, err := m.match(input, restpattern)
			if err != nil {
				return nil, false, err
			} else if !ok {
				return nil, false, nil
			} else {
				// If the input is not a pair and it matches restpattern, we are done.
				result = restResult
				break
			}
		}
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

func PatternVariables(pattern Datum, literals map[Symbol]Location) (map[Symbol]int, error) {
	switch p := pattern.(type) {
	case Boolean, Number, Character, String, nil:
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
		if p == Underscore || p == Ellipsis {
			return nil, nil
		}
		return nil, fmt.Errorf("match: unhandled pattern %#v", p)
	}
}

func MatchSyntax(input Datum, pattern Datum, literals map[Symbol]Location) (map[Symbol]interface{}, bool, error) {
	return newSyntaxMatcher(literals).match(input, pattern)
}