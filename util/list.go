package util

import (
	"fmt"

	"github.com/katsuya94/grime/common"
)

var ErrImproperList = fmt.Errorf("improper list")

func Each(list common.Datum, fn func(common.Datum) error) error {
	for {
		if p, ok := list.(common.Pair); ok {
			err := fn(p.First)
			if err != nil {
				return err
			}
			list = p.Rest
			continue
		} else if list == common.Null {
			break
		}
		return ErrImproperList
	}
	return nil
}

func Slice(list common.Datum) ([]common.Datum, error) {
	var data []common.Datum
	for {
		if p, ok := list.(common.Pair); ok {
			data = append(data, p.First)
			list = p.Rest
			continue
		} else if list == common.Null {
			break
		}
		return nil, ErrImproperList
	}
	return data, nil
}

func List(data ...common.Datum) common.Datum {
	if len(data) == 0 {
		return common.Null
	} else {
		return common.Pair{data[0], List(data[1:]...)}
	}
}
