package test

import (
	"fmt"
	"runtime"
	"strings"
	"testing"

	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/lib"
	"github.com/katsuya94/grime/read"
	"github.com/stretchr/testify/require"
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

func Grime(t *testing.T, source string) {
	_, file, line, ok := runtime.Caller(1)
	if !ok {
		panic("can't determine caller")
	}
	filename := fmt.Sprintf("%v:%v:github.com/katsuya94/grime/test.Grime", file, line)
	topLevelProgram, nullSourceLocationTree, err := read.Read(filename, strings.NewReader(source))
	require.NoError(t, err)
	err = lib.Runtime.Execute(topLevelProgram, nullSourceLocationTree)
	require.NoError(t, err)
}
