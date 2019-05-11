package grime_test

import (
	"testing"

	"github.com/katsuya94/grime/lib/base"
	"github.com/katsuya94/grime/lib/core"
	"github.com/katsuya94/grime/lib/derived"
	. "github.com/katsuya94/grime/lib/grime"
	"github.com/katsuya94/grime/runtime"
)

func TestGrime(t *testing.T) {
	rt := runtime.NewRuntime(core.NewCompiler())
	rt.MustProvide(core.Library)
	rt.MustBind(core.Library.Name(), core.Bindings)
	rt.MustProvide(derived.Library)
	rt.MustProvide(base.Library)
	rt.MustProvide(Library)
	err := rt.ExecuteFile("grime_test")
	if err != nil {
		t.Fatal(err)
	}
}
