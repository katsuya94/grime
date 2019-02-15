package common

func Body(nullSourceLocationTree SourceLocationTree, syntaxes ...Syntax) Syntax {
	list := NewSyntax(NewWrappedSyntax(Null, &nullSourceLocationTree))
	for i := len(syntaxes) - 1; i >= 0; i-- {
		var sourceLocationTree *SourceLocationTree
		if syntaxes[i].SourceLocationTree() != nil {
			sourceLocationTree = &SourceLocationTree{
				SourceLocation{
					File:   syntaxes[i].SourceLocation().File,
					Line:   syntaxes[i].SourceLocation().Line,
					Column: syntaxes[i].SourceLocation().Column,
					Offset: syntaxes[i].SourceLocation().Offset,
					Length: list.SourceLocation().Offset + list.SourceLocation().Length - syntaxes[i].SourceLocation().Offset,
				},
				Pair{*syntaxes[i].SourceLocationTree(), *list.SourceLocationTree()},
			}
		}
		list = NewSyntax(NewWrappedSyntax(Pair{syntaxes[i].Unwrap(), list.Unwrap()}, sourceLocationTree))
	}
	return list
}
