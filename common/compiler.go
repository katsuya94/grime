package common

type Compiler func(Syntax, Scope) (Expression, error)
