package util

import (
	"fmt"

	"github.com/katsuya94/grime/common"
)

func Pattern(d common.Datum) common.Datum {
	if d, ok := d.(common.Pair); ok {
		return common.Pair{
			Pattern(d.First),
			Pattern(d.Rest),
		}
	}
	if d == common.Symbol("_") {
		return common.Wildcard
	}
	if d == common.Symbol("...") {
		return common.Ellipsis
	}
	return d
}

type syntaxMatcher struct {
	literals map[common.Symbol]common.Location
}

func newSyntaxMatcher(literals map[common.Symbol]common.Location) *syntaxMatcher {
	return &syntaxMatcher{literals}
}

func (m *syntaxMatcher) match(input common.Datum, pattern common.Datum) (map[common.Symbol]interface{}, bool, error) {
	syntax, isSyntax := input.(common.WrappedSyntax)
	switch p := pattern.(type) {
	case common.Boolean, common.Number, common.Character, common.String, nil:
		if (isSyntax && syntax.Datum() == p) || input == p {
			return map[common.Symbol]interface{}{}, true, nil
		}
		return nil, false, nil
	case common.Symbol:
		if location, ok := m.literals[p]; ok {
			if !isSyntax {
				return nil, false, nil
			}
			name, l, ok := syntax.Identifier()
			if !ok {
				return nil, false, nil
			} else if location == nil && name == p {
				return map[common.Symbol]interface{}{}, true, nil
			} else if location != nil && l == location {
				return map[common.Symbol]interface{}{}, true, nil
			}
			return nil, false, nil
		}
		return map[common.Symbol]interface{}{p: input}, true, nil
	case common.Pair:
		if rest, ok := p.Rest.(common.Pair); ok {
			if rest.First == common.Ellipsis {
				return m.matchEllipsis(input, p.First, rest.Rest)
			}
		}
		var (
			first common.Datum
			rest  common.Datum
		)
		if isSyntax {
			pair, ok := syntax.Datum().(common.Pair)
			if !ok {
				return nil, false, nil
			}
			first = syntax.PushOnto(pair.First)
			rest = syntax.PushOnto(pair.Rest)
		} else {
			pair, ok := input.(common.Pair)
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
		result := map[common.Symbol]interface{}{}
		for k, v := range firstResult {
			result[k] = v
		}
		for k, v := range restResult {
			result[k] = v
		}
		return result, true, nil
	default:
		if p == common.Wildcard {
			return map[common.Symbol]interface{}{}, true, nil
		}
		return nil, false, fmt.Errorf("match: unhandled pattern %#v", p)
	}
}

func (m *syntaxMatcher) matchEllipsis(input common.Datum, subpattern common.Datum, restpattern common.Datum) (map[common.Symbol]interface{}, bool, error) {
	var (
		result     map[common.Symbol]interface{}
		subresults []map[common.Symbol]interface{}
	)
	for {
		syntax, isSyntax := input.(common.WrappedSyntax)
		var (
			pair   common.Pair
			isPair bool
			first  common.Datum
			rest   common.Datum
		)
		if isSyntax {
			pair, isPair = syntax.Datum().(common.Pair)
			if isPair {
				first = syntax.PushOnto(pair.First)
				rest = syntax.PushOnto(pair.Rest)
			}
		} else {
			pair, isPair = input.(common.Pair)
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

func PatternVariables(pattern common.Datum, literals map[common.Symbol]common.Location) (map[common.Symbol]int, error) {
	switch p := pattern.(type) {
	case common.Boolean, common.Number, common.Character, common.String, nil:
		return nil, nil
	case common.Symbol:
		if _, ok := literals[p]; ok {
			return nil, nil
		} else {
			return map[common.Symbol]int{p: 0}, nil
		}
	case common.Pair:
		if rest, ok := p.Rest.(common.Pair); ok {
			if rest.First == common.Ellipsis {
				subpatternVariables, err := PatternVariables(p.First, literals)
				if err != nil {
					return nil, err
				}
				patternVariables := make(map[common.Symbol]int, len(subpatternVariables))
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
		patternVariables := make(map[common.Symbol]int, len(firstPatternVariables)+len(restPatternVariables))
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
		if p == common.Wildcard || p == common.Ellipsis {
			return nil, nil
		}
		return nil, fmt.Errorf("match: unhandled pattern %#v", p)
	}
}

func MatchSyntax(input common.Datum, pattern common.Datum, literals map[common.Symbol]common.Location) (map[common.Symbol]interface{}, bool, error) {
	return newSyntaxMatcher(literals).match(input, pattern)
}
