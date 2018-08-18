package core

import "fmt"

type Datum interface{}

type Boolean bool
type Number string
type Character rune
type String string
type Symbol string
type Pair struct {
	First Datum
	Rest  Datum
}
type WrappedSyntax struct{
	Form Datum
	// TODO add marks and substitutions
}
type Procedure struct {}

func Display(d Datum) string {
	switch v := d.(type) {
	case Boolean:
		if v {
			return "#t"
		} else {
			return "#f"
		}
	case Number:
		return string(v)
	case Character:
		return fmt.Sprintf(`#\%v`, string(v))
	case String:
		return fmt.Sprintf(`"%v"`, string(v))
	case Symbol, Pair, nil:
		return fmt.Sprintf("'%v", fmtQuoted(v))
	case WrappedSyntax:
		return fmt.Sprintf(`#<syntax %v>`, Display(v.Form))
	default:
		panic("unhandled datum")
	}
}

func fmtQuoted(d Datum) string {
	switch v := d.(type) {
	case Symbol:
		return string(v)
	case Pair:
		return fmt.Sprintf("(%v%v", fmtQuoted(v.First), fmtRest(v.Rest))
	case nil:
		return "()"
	default:
		return Display(d)
	}
}

func fmtRest(d Datum) string {
	switch v := d.(type) {
	case Pair:
		return fmt.Sprintf(" %v%v", fmtQuoted(v.First), fmtRest(v.Rest))
	case nil:
		return ")"
	default:
		return fmt.Sprintf(" . %v)", fmtQuoted(v))
	}
}
