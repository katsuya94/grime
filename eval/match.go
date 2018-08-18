package eval

import (
	"github.com/katsuya94/grime/core"
	"fmt"
)

func Match(input core.Datum, pattern core.Datum, literals map[core.Symbol]Binding) (map[core.Symbol]interface{}, bool, error) {
	switch v := p.pattern.(type) {
	case core.Symbol:
		if binding, ok := p.literals[v]; ok {
			return nil, false, Errorf("match: matching literals not implemented")
		} else {

		}
	default:
		return nil, false, Errorf("match: unhandled datum %#v", v)
	}
}