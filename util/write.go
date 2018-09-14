package util

import (
	"fmt"

	"github.com/katsuya94/grime/common"
)

func Write(d common.Datum) string {
	switch v := d.(type) {
	case common.Boolean:
		if v {
			return "#t"
		} else {
			return "#f"
		}
	case common.Number:
		return string(v)
	case common.Character:
		return fmt.Sprintf(`#\%v`, string(v))
	case common.String:
		return fmt.Sprintf(`"%v"`, string(v))
	case common.Symbol:
		return string(v)
	case common.Pair:
		return fmt.Sprintf("(%v%v", Write(v.First), fmtRest(v.Rest))
	case nil:
		return "()"
	case common.Quote:
		return fmt.Sprintf("'%v", Write(v.Datum))
	default:
		if v == common.Void {
			return "#<void>"
		}
		panic("unhandled datum")
	}
}

func fmtRest(d common.Datum) string {
	switch v := d.(type) {
	case common.Pair:
		return fmt.Sprintf(" %v%v", Write(v.First), fmtRest(v.Rest))
	case nil:
		return ")"
	default:
		return fmt.Sprintf(" . %v)", Write(v))
	}
}
