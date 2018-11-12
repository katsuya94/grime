package main

import (
	"bufio"
	"bytes"
	"fmt"
	"io"
	"os"
	"syscall"

	"github.com/katsuya94/grime/eval"

	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/lib/base"
	"github.com/katsuya94/grime/lib/core"
	"github.com/katsuya94/grime/lib/derived"
	"github.com/katsuya94/grime/lib/grime"
	"github.com/katsuya94/grime/read"
	"github.com/katsuya94/grime/runtime"
	"golang.org/x/crypto/ssh/terminal"
)

func main() {
	if terminal.IsTerminal(syscall.Stdin) {
		repl()
	} else {
		if err := run(); err != nil {
			fmt.Fprintln(os.Stderr, err)
			os.Exit(1)
		}
	}
}

type replEvaluated struct{}

func repl() {
	rt := runtime.NewRuntime()
	rt.Provide(core.Library)
	rt.Bind(core.Library.Name(), core.Bindings)
	rt.Provide(derived.Library)
	rt.Provide(base.Library)
	rt.Provide(grime.Library)
	rt.Instantiate([]common.Symbol{common.Symbol("grime")})
	bindings, err := rt.BindingsFor([]common.Symbol{common.Symbol("grime")})
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	var forms []common.Form
	for {
		fmt.Printf("grime:%v> ", len(forms))
		eof := false
		data, err := readReplData()
		if err == io.EOF {
			if len(forms) == 0 {
				break
			} else {
				eof = true
			}
		} else if err != nil {
			forms = nil
			fmt.Fprintf(os.Stderr, "error: %v\n", err)
			continue
		}
		for _, d := range data {
			forms = append(forms, common.NewWrappedSyntax(d))
		}
		env := common.NewEnvironment(bindings)
		expression, newBindings, err := eval.CompileBody(env, forms)
		if err == eval.ErrNoExpressionsInBody && !eof {
			continue
		}
		forms = nil
		if err != nil {
			fmt.Fprintf(os.Stderr, "error: %v\n", err)
			continue
		}
		bindings = newBindings
		result, err := eval.EvaluateExpressionOnce(expression)
		if err != nil {
			fmt.Fprintf(os.Stderr, "error: %v\n", err)
			continue
		}
		fmt.Println(common.Write(result))
	}
}

func readReplData() ([]common.Datum, error) {
	var source []byte
	scanner := bufio.NewScanner(os.Stdin)
	scanner.Split(splitReplLines)
	for {
		if scanner.Scan() {
			source = append(source, scanner.Bytes()...)
			if data, err := read.ReadBytes(source); err == nil {
				return data, nil
			} else if err != read.ErrUnexpectedEOF {
				return nil, err
			}
		} else if err := scanner.Err(); err != nil {
			return nil, err
		} else {
			source = append(source, scanner.Bytes()...)
			if data, err := read.ReadBytes(source); data == nil && err == nil {
				return nil, io.EOF
			} else {
				return data, err
			}
		}
	}
}

func splitReplLines(data []byte, atEOF bool) (int, []byte, error) {
	if atEOF && len(data) == 0 {
		return 0, nil, nil
	}
	if i := bytes.IndexByte(data, '\n'); i >= 0 {
		return i + 1, data[0 : i+1], nil
	}
	if atEOF {
		return len(data), data, nil

	}
	return 0, nil, nil
}

func run() error {
	r := runtime.NewRuntime()
	r.Provide(core.Library)
	r.Bind(core.Library.Name(), core.Bindings)
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
	return r.Execute(topLevelProgram)
}
