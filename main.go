package main

import (
	"fmt"
	"io"
	"os"
	"syscall"

	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/lib/base"
	"github.com/katsuya94/grime/lib/core"
	"github.com/katsuya94/grime/lib/derived"
	"github.com/katsuya94/grime/lib/grime"
	"github.com/katsuya94/grime/read"
	"github.com/katsuya94/grime/repl"
	"github.com/katsuya94/grime/runtime"
	"golang.org/x/crypto/ssh/terminal"
)

func main() {
	err := run()
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
}

func run() error {
	rt := runtime.NewRuntime()
	rt.Provide(core.Library)
	rt.Bind(core.Library.Name(), core.Bindings)
	rt.Provide(derived.Library)
	rt.Provide(base.Library)
	rt.Provide(grime.Library)
	if terminal.IsTerminal(syscall.Stdin) {
		return interactive(rt)
	} else {
		return topLevelProgram(rt)
	}
}

func interactive(rt *runtime.Runtime) error {
	rt.Instantiate([]common.Symbol{common.Symbol("grime")})
	bindings, err := rt.BindingsFor([]common.Symbol{common.Symbol("grime")})
	if err != nil {
		return err
	}
	repl.REPL(bindings, os.Stdin, os.Stdout)
	return nil
}

func topLevelProgram(rt *runtime.Runtime) error {
	reader := read.NewDatumReader(os.Stdin)
	var topLevelProgram []common.WrappedSyntax
	for {
		if datum, err := reader.ReadDatum(); err == io.EOF {
			break
		} else if err != nil {
			return err
		} else {
			topLevelProgram = append(topLevelProgram, common.NewWrappedSyntax(datum))
		}
	}
	return rt.Execute(topLevelProgram)
}
