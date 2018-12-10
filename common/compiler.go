package common

type Compiler func(WrappedSyntax) (Expression, []WrappedSyntax, error)
