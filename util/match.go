package util

import (
	"fmt"

	"github.com/katsuya94/grime/common"
)

type matcher struct {
	literals map[common.Symbol]common.Binding
}

func newMatcher(literals map[common.Symbol]common.Binding) *matcher {
	return &matcher{literals}
}

func (m *matcher) match(input common.Datum, pattern common.Datum, ellipsis bool) (map[common.Symbol]interface{}, bool, error) {
	switch p := pattern.(type) {
	case common.Boolean, common.Number, common.Character, common.String, nil:
		if input == p {
			return map[common.Symbol]interface{}{}, true, nil
		} else {
			return nil, false, nil
		}
	case common.Symbol:
		if _, ok := m.literals[p]; ok {
			// TODO implement lexical literal matching
			if s, ok := input.(common.Symbol); !ok {
				return nil, false, nil
			} else if s == p {
				return map[common.Symbol]interface{}{}, true, nil
			} else {
				return nil, false, nil
			}
		} else if p == common.Symbol("...") { // TODO check lexical scope
			return nil, false, fmt.Errorf("match: unexpected ellipsis")
		} else if p == common.Symbol("_") { // TODO check lexical scope
			return map[common.Symbol]interface{}{}, true, nil
		} else {
			return map[common.Symbol]interface{}{p: input}, true, nil
		}
	case common.Pair:
		if rest, ok := p.Rest.(common.Pair); ok {
			// TODO check lexical scope
			if symbol, ok := rest.First.(common.Symbol); ok && symbol == common.Symbol("...") && ellipsis {
				return m.matchEllipsis(input, p.First, rest.Rest)
			}
		}
		if i, ok := input.(common.Pair); ok {
			firstResult, match, err := m.match(i.First, p.First, true)
			if err != nil {
				return nil, false, err
			} else if !match {
				return nil, false, nil
			}
			restResult, match, err := m.match(i.Rest, p.Rest, ellipsis)
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
		} else {
			return nil, false, nil
		}
	default:
		return nil, false, fmt.Errorf("match: unhandled pattern %#v", p)
	}
}

func (m *matcher) matchEllipsis(input common.Datum, subpattern common.Datum, restpattern common.Datum) (map[common.Symbol]interface{}, bool, error) {
	var (
		result     map[common.Symbol]interface{}
		subresults []map[common.Symbol]interface{}
	)
	for {
		if i, ok := input.(common.Pair); ok {
			if subresult, ok, err := m.match(i.First, subpattern, true); err != nil {
				return nil, false, err
			} else if !ok {
				restResult, ok, err := m.match(i, restpattern, false)
				if err != nil {
					return nil, false, err
				} else if !ok {
					return nil, false, nil
				} else {
					// If the first does not match subpattern, but the pair matches restpattern, we are done.
					result = restResult
					break
				}
			} else {
				subresults = append(subresults, subresult)
				restResult, ok, err := m.match(i.Rest, restpattern, false)
				if err != nil {
					return nil, false, nil
				} else if !ok {
					// If the rest does not yet match restpattern, continue.
					input = i.Rest
				} else if _, ok, err := m.matchEllipsis(i.Rest, subpattern, restpattern); err != nil {
					return nil, false, err
				} else if ok {
					// If the rest still matches subpattern and restpattern, continue.
					input = i.Rest
				} else {
					// If the rest does not match subpattern and restpattern, we are done.
					result = restResult
					break
				}
			}
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
	if patternVariables, err := m.patternVariables(subpattern); err != nil {
		return nil, false, err
	} else {
		for _, patternVariable := range patternVariables {
			result[patternVariable] = []interface{}{}
		}
	}
	for _, subresult := range subresults {
		for k, v := range subresult {
			result[k] = append(result[k].([]interface{}), v)
		}
	}
	return result, true, nil
}

func (m *matcher) patternVariables(pattern common.Datum) ([]common.Symbol, error) {
	switch p := pattern.(type) {
	case common.Boolean, common.Number, common.Character, common.String, nil:
		return nil, nil
	case common.Symbol:
		if _, ok := m.literals[p]; ok {
			return nil, nil
		} else if p == common.Symbol("_") || p == common.Symbol("...") {
			return nil, nil
		} else {
			return []common.Symbol{p}, nil
		}
	case common.Pair:
		firstPatternVariables, err := m.patternVariables(p.First)
		if err != nil {
			return nil, err
		}
		restPatternVariables, err := m.patternVariables(p.Rest)
		if err != nil {
			return nil, err
		}
		return append(firstPatternVariables, restPatternVariables...), nil
	default:
		return nil, fmt.Errorf("match: unhandled pattern %#v", p)
	}
}

func Match(input common.Datum, pattern common.Datum, literals map[common.Symbol]common.Binding) (map[common.Symbol]interface{}, bool, error) {
	return newMatcher(literals).match(input, pattern, true)
}
