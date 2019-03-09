package test

import (
	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/read"
)

func Syntax(s string) common.Syntax {
	return read.MustReadSyntax(s)
}

func Identifier(s string) common.Identifier {
	id, ok := Syntax(s).Identifier()
	if !ok {
		panic("not an identifier")
	}
	return id
}

func WithBinding(id common.Identifier, location common.Location, syntax common.Syntax) common.Syntax {
	scope := common.NewScope()
	err := scope.Set(id, location)
	if err != nil {
		panic(err)
	}
	return syntax.Push(scope, common.LEXICAL)
}
