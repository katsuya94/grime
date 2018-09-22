package common

func Write(d Datum) string {
	if d == nil {
		return "()"
	} else {
		return d.Write()
	}
}
