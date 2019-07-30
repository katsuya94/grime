package common

import "fmt"

type Writer interface {
	Datum
	Write() string
}

func Write(d Datum) string {
	w, ok := d.(Writer)
	if !ok {
		return fmt.Sprintf("#<%T>", d)
	}
	return w.Write()
}

type PrettyPrinter interface {
	Datum
	PrettyPrint(int) string
}

func PrettyPrint(d Datum, indent int) string {
	pp, ok := d.(PrettyPrinter)
	if !ok {
		return Write(d)
	}
	return pp.PrettyPrint(indent)
}
