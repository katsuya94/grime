package common

import "fmt"

type Writable interface {
	Datum
	Write() string
}

func Write(d Datum) string {
	w, ok := d.(Writable)
	if !ok {
		return fmt.Sprintf("#<%T>", d)
	}
	return w.Write()
}
