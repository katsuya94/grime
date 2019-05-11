package common

type Compiler interface {
	Compile(Syntax, Scope, *FrameTemplate) (Expression, error)
}
