package common

type Compiler interface {
	Compile(Syntax, Scope, *FrameTemplate, Stack) (Expression, error)
}
