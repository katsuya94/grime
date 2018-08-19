package eval

import (
	"github.com/katsuya94/grime/core"
	"fmt"
)

type matcher struct {
	literals map[core.Symbol]Binding
}

func newMatcher(literals map[core.Symbol]Binding) *matcher {
	return &matcher{literals}
}

func (m *matcher) match(input core.Datum, pattern core.Datum, ellipsis bool) (map[core.Symbol]interface{}, bool, error) {
	switch p := pattern.(type) {
	case core.Boolean, core.Number, core.Character, core.String, nil:
		if input == p {
			return map[core.Symbol]interface{}{}, true, nil
		} else {
			return nil, false, nil
		}
	case core.Symbol:
		if _, ok := m.literals[p]; ok {
			return nil, false, Errorf("match: matching literals not implemented")
		} else if p == core.Symbol("...") { // TODO does this need to check lexical scope?
			return nil, false, Errorf("match: unexpected ellipsis")
		} else if p == core.Symbol("_") { // TODO does this need to check lexical scope?
			return map[core.Symbol]interface{}{}, true, nil
		} else {
			return map[core.Symbol]interface{}{p: input}, true, nil
		}
	case core.Pair:
		if rest, ok := p.Rest.(core.Pair); ok {
			// TODO does this need to check lexical scope?
			if symbol, ok := rest.First.(core.Symbol); ok && symbol == core.Symbol("...") && ellipsis {
				return m.matchEllipsis(input, p.First, rest.Rest)
			}
		}
		if i, ok := input.(core.Pair); ok {
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
			result := map[core.Symbol]interface{}{}
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
		return nil, false, Errorf("match: unhandled pattern %#v", p)
	}
}

func (m *matcher) matchEllipsis(input core.Datum, subpattern core.Datum, restpattern core.Datum) (map[core.Symbol]interface{}, bool, error) {
	var (
		result     map[core.Symbol]interface{}
		subresults []map[core.Symbol]interface{}
	)
	for {
		fmt.Printf("HERE %#v\n", input)
		fmt.Printf("BEAR %#v\n", subpattern)
		switch i := input.(type) {
		case core.Boolean, core.Number, core.Character, core.String, core.Symbol, nil:
			return m.match(i, restpattern, false)
		case core.Pair:
			if subresult, ok, err := Match(i.First, subpattern, m.literals); err != nil {
				return nil, false, err
			} else if !ok {
				restResult, match, err := m.match(i, restpattern, false)
				if err != nil {
					return nil, false, err
				} else if !match {
					return nil, false, nil
				}
				result = restResult
			} else {
				subresults = append(subresults, subresult)
				restResult, ok, err := m.match(i.Rest, restpattern, false)
				if err != nil {
					return nil, false, nil
				} else if !ok{
					input = i.Rest
					continue
				} else if rest, ok := i.Rest.(core.Pair); ok {
					if _, ok, err := m.match(rest, restpattern, false); err != nil {
						return nil, false, err
					} else if ok {
						input = i.Rest
						continue
					}
				}
				result = restResult
			}
		}
		break
	}
	if len(subresults) > 0  {
		for k, _ := range subresults[0] {
			result[k] = []interface{}{}
		}
	}
	for _, subresult := range subresults {
		for k, v := range subresult {
			result[k] = append(result[k].([]interface{}), v)
		}
	}
	return result, true, nil
}

func Match(input core.Datum, pattern core.Datum, literals map[core.Symbol]Binding) (map[core.Symbol]interface{}, bool, error) {
	return newMatcher(literals).match(input, pattern, true)
}