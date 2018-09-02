package util

import (
	"fmt"

	"github.com/katsuya94/grime/common"
)

var ErrImproperList = fmt.Errorf("improper list")

func Each(list common.Datum, fn func(common.Datum) error) error {
	for {
		if p, ok := list.(common.Pair); ok {
			if err := fn(p.First); err != nil {
				return err
			} else {
				list = p.Rest
			}
		} else if list == nil {
			return nil
		} else {
			return ErrImproperList
		}
	}
}

func List(data ...common.Datum) common.Datum {
	if len(data) == 0 {
		return nil
	} else {
		return common.Pair{data[0], List(data[1:]...)}
	}
}
