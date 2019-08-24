package r6rs_base

import (
	"github.com/katsuya94/grime/common"
)

func idDatumSlice(ids []common.Identifier) []common.Datum {
	data := make([]common.Datum, len(ids))
	for i := range ids {
		data[i] = ids[i].WrappedSyntax
	}
	return data
}

func syntaxDatumSlice(syntaxes []common.Syntax) []common.Datum {
	data := make([]common.Datum, len(syntaxes))
	for i := range syntaxes {
		data[i] = syntaxes[i].Datum()
	}
	return data
}

func introduce(datum common.Datum) common.Datum {
	return Introduce(common.NewSyntax(common.NewWrappedSyntax(datum, nil))).Datum()
}

func list(data ...common.Datum) common.Datum {
	if len(data) == 0 {
		return introduce(common.Null)
	} else {
		return common.Pair{data[0], list(data[1:]...)}
	}
}

func syntaxKeywordForErrMsg(syntax common.Syntax) string {
	pair, ok := syntax.Pair()
	if !ok {
		return "(unknown)"
	}
	id, ok := common.NewSyntax(pair.First).Identifier()
	if !ok {
		return "(unknown)"
	}
	return string(id.Name())
}
