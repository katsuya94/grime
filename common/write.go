package common

import "fmt"

func Write(d Datum) string {
	w, ok := d.(Writable)
	if !ok {
		return fmt.Sprintf("#<%T>", d)
	}
	return w.Write()
}
