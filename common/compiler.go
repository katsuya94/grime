package common

type Compiler func(Syntax, Scope) (Expression, FrameTemplate, error)
