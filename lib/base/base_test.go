package base_test

import (
	"os"
	"path/filepath"
	"runtime"
	"testing"

	"github.com/katsuya94/grime/common"
	. "github.com/katsuya94/grime/lib/base"
	"github.com/katsuya94/grime/lib/core"
	"github.com/katsuya94/grime/read"
	grime "github.com/katsuya94/grime/runtime"
)

func TestBase(t *testing.T) {
	_, filename, _, ok := runtime.Caller(0)
	if !ok {
		t.Fatal()
	}
	sourcePath := filepath.Join(filepath.Dir(filename), "base_test.scm")
	f, err := os.Open(sourcePath)
	if err != nil {
		t.Fatal(err)
	}
	data, err := read.Read(f)
	if err != nil {
		t.Fatal(err)
	}
	runtime := grime.NewRuntime()
	runtime.Provide(core.Library)
	runtime.Bind(core.Library.Name(), core.Bindings)
	runtime.Provide(Library)
	var topLevelProgram []common.WrappedSyntax
	for _, d := range data {
		topLevelProgram = append(topLevelProgram, common.NewWrappedSyntax(d))
	}
	err = runtime.Execute(topLevelProgram)
	if err != nil {
		t.Fatal(err)
	}
}
