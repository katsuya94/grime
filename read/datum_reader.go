package read

import "github.com/katsuya94/grime/common"

func MustReadData(s string) []common.Datum {
	syntaxes, _ := MustReadSyntaxes(s)
	var data []common.Datum
	for _, syntax := range syntaxes {
		data = append(data, syntax.Unwrap())
	}
	return data
}

func MustReadDatum(s string) common.Datum {
	return MustReadSyntax(s).Unwrap()
}
