package eval

import "github.com/katsuya94/grime/core"

type Pattern struct {
	literals map[core.Symbol]Binding
	pattern  core.Datum
}

func Match(in core.Datum, p Pattern) (map[core.Symbol]interface{}, bool, error) {
	return nil, false, nil
}
