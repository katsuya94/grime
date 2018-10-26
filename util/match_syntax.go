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

func (m *syntaxMatcher) match(input common.WrappedSyntax, pattern common.Datum, ellipsis bool) (map[common.Symbol]interface{}, bool, error) {
	switch p := pattern.(type) {
	case common.Boolean, common.Number, common.Character, common.String, nil:
		if input.Datum() == p {
			return map[common.Symbol]interface{}{}, true, nil
		}
		return nil, false, nil
	case common.Symbol:
		if location, ok := m.literals[p]; ok {
			name, l, ok := input.Identifier()
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
			if rest.First == common.Ellipsis && ellipsis {
				return m.matchEllipsis(input, p.First, rest.Rest)
			}
		}
		if i, ok := input.Datum().(common.Pair); ok {
			firstResult, match, err := m.match(input.PushOnto(i.First), p.First, true)
			if err != nil {
				return nil, false, err
			} else if !match {
				return nil, false, nil
			}
			restResult, match, err := m.match(input.PushOnto(i.Rest), p.Rest, ellipsis)
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
		}
		return nil, false, nil
	default:
		if p == common.Wildcard {
			return map[common.Symbol]interface{}{}, true, nil
		}
		return nil, false, fmt.Errorf("match: unhandled pattern %#v", p)
	}
}

func (m *syntaxMatcher) matchEllipsis(input common.WrappedSyntax, subpattern common.Datum, restpattern common.Datum) (map[common.Symbol]interface{}, bool, error) {
	var (
		result     map[common.Symbol]interface{}
		subresults []map[common.Symbol]interface{}
	)
	for {
		if i, ok := input.Datum().(common.Pair); ok {
			subresult, ok, err := m.match(input.PushOnto(i.First), subpattern, true)
			if err != nil {
				return nil, false, err
			} else if !ok {
				restResult, ok, err := m.match(input, restpattern, false)
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
			restResult, ok, err := m.match(input.PushOnto(i.Rest), restpattern, false)
			if err != nil {
				return nil, false, nil
			} else if ok {
				_, ok, err := m.matchEllipsis(input.PushOnto(i.Rest), subpattern, restpattern)
				if err != nil {
					return nil, false, err
				} else if !ok {
					// If the rest matches the restpattern, but does not match the ellipsis, we are done.
					result = restResult
					break
				}
			}
			// Otherwise continue.
			input = input.PushOnto(i.Rest)
		} else {
			restResult, ok, err := m.match(input, restpattern, false)
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
	}
	patternVariables, err := PatternVariables(subpattern, m.literals)
	if err != nil {
		return nil, false, err
	}
	for _, patternVariable := range patternVariables {
		result[patternVariable] = []interface{}{}
	}
	for _, subresult := range subresults {
		for k, v := range subresult {
			result[k] = append(result[k].([]interface{}), v)
		}
	}
	return result, true, nil
}

func PatternVariables(pattern common.Datum, literals map[common.Symbol]common.Location) ([]common.Symbol, error) {
	switch p := pattern.(type) {
	case common.Boolean, common.Number, common.Character, common.String, nil:
		return nil, nil
	case common.Symbol:
		if _, ok := literals[p]; ok {
			return nil, nil
		} else {
			return []common.Symbol{p}, nil
		}
	case common.Pair:
		firstPatternVariables, err := PatternVariables(p.First, literals)
		if err != nil {
			return nil, err
		}
		restPatternVariables, err := PatternVariables(p.Rest, literals)
		if err != nil {
			return nil, err
		}
		return append(firstPatternVariables, restPatternVariables...), nil
	default:
		if p == common.Wildcard || p == common.Ellipsis {
			return nil, nil
		}
		return nil, fmt.Errorf("match: unhandled pattern %#v", p)
	}
}

func MatchSyntax(input common.WrappedSyntax, pattern common.Datum, literals map[common.Symbol]common.Location) (map[common.Symbol]interface{}, bool, error) {
	return newSyntaxMatcher(literals).match(input, pattern, true)
}
