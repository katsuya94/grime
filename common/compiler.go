package common

type Compiler func(WrappedSyntax, *Scope) (Expression, error)
