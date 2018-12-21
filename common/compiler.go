package common

type Compiler func(Datum) (Expression, []WrappedSyntax, error)
