package common

func Body(syntaxes ...WrappedSyntax) WrappedSyntax {
	list := NewWrappedSyntax(Null, nil)
	for i := len(syntaxes) - 1; i >= 0; i-- {
		sourceLocationTree := SourceLocationTree{
			SourceLocation{
				File:   syntaxes[i].SourceLocation().File,
				Line:   syntaxes[i].SourceLocation().Line,
				Column: syntaxes[i].SourceLocation().Column,
				Offset: syntaxes[i].SourceLocation().Offset,
				Length: list.SourceLocation().Offset + list.SourceLocation().Length - syntaxes[i].SourceLocation().Offset,
			},
			Pair{syntaxes[i].SourceLocationTree(), list.SourceLocationTree()},
		}
		list = NewWrappedSyntax(Pair{syntaxes[i].Datum, list.Datum()}, &sourceLocationTree)
	}
	return list
}
