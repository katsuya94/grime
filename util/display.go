package util

import (
	"fmt"

	"github.com/katsuya94/grime/common"
)

func Display(d common.Datum) string {
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
	case common.Symbol, common.Pair, nil:
		return fmt.Sprintf("'%v", fmtQuoted(v))
	case common.WrappedSyntax:
		return fmt.Sprintf(`#<syntax %v>`, Display(v.Form))
	default:
		panic("unhandled datum")
	}
}

func fmtQuoted(d common.Datum) string {
	switch v := d.(type) {
	case common.Symbol:
		return string(v)
	case common.Pair:
		return fmt.Sprintf("(%v%v", fmtQuoted(v.First), fmtRest(v.Rest))
	case nil:
		return "()"
	default:
		return Display(d)
	}
}

func fmtRest(d common.Datum) string {
	switch v := d.(type) {
	case common.Pair:
		return fmt.Sprintf(" %v%v", fmtQuoted(v.First), fmtRest(v.Rest))
	case nil:
		return ")"
	default:
		return fmt.Sprintf(" . %v)", fmtQuoted(v))
	}
}
