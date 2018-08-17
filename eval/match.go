package eval

import (
	"github.com/katsuya94/grime/core"
	"fmt"
)

type Pattern struct {
	literals map[core.Symbol]Binding
	pattern  core.Datum
}

func Match(in core.Datum, p Pattern) (map[core.Symbol]interface{}, bool, error) {
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