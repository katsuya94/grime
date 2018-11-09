package main

import (
	"bufio"
	"bytes"
	"fmt"
	"io"
	"os"
	"syscall"

	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/lib/base"
	"github.com/katsuya94/grime/lib/core"
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

func repl() {
	rt := runtime.NewRuntime()
	rt.Provide(core.Library)
	rt.Bind(core.Library.Name(), core.Bindings)
	rt.Provide(base.Library)
	for {
		fmt.Print("grime> ")
		data, err := readReplData()
		if err == io.EOF {
			break
		} else if err != nil {
			fmt.Fprintf(os.Stderr, "error: %v\n", err)
		}
		data = append(read.MustReadString("(import (base))"), data...)
		var topLevelProgram []common.WrappedSyntax
		for _, d := range data {
			topLevelProgram = append(topLevelProgram, common.NewWrappedSyntax(d))
		}
		err = rt.Execute(topLevelProgram)
		if err != nil {
			fmt.Fprintf(os.Stderr, "error: %v\n", err)
		}
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
