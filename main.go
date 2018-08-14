package main

import (
	"fmt"
	"github.com/katsuya94/grime/core"
	"github.com/katsuya94/grime/eval"
	"github.com/katsuya94/grime/read"
	"github.com/katsuya94/grime/repl"
	"golang.org/x/crypto/ssh/terminal"
	"io"
	"os"
	"syscall"
)

func main() {
	var err error
	if terminal.IsTerminal(syscall.Stdin) {
		err = repl.NewREPL().Run()
	} else {
		err = run()
	}
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
}

func run() error {
	var (
		topLevelProgram []core.Datum
		reader          = read.NewDatumReader(os.Stdin)
		env             = eval.NewEnvironment()
	)
	for {
		if datum, err := reader.ReadDatum(); err == io.EOF {
			break
		} else if err != nil {
			return err
		} else {
			topLevelProgram = append(topLevelProgram, datum)
		}
	}
	return env.EvaluateTopLevelProgram(topLevelProgram)
}
