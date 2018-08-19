package main

import (
	"bufio"
	"bytes"
	"fmt"
	"github.com/katsuya94/grime/core"
	"github.com/katsuya94/grime/eval"
	"github.com/katsuya94/grime/read"
	"golang.org/x/crypto/ssh/terminal"
	"io"
	"os"
	"syscall"
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
	env := eval.NewEnvironment()
	for {
		fmt.Print("grime> ")
		body, err := readReplData()
		if err == io.EOF {
			break
		} else if err != nil {
			fmt.Fprintf(os.Stderr, "error: %v\n", err)
		}
		expression, err := eval.ExpandBody(env, body)
		if err != nil {
			fmt.Fprintf(os.Stderr, "error: %v\n", err)
		}
		value, err := eval.EvaluateExpression(env, expression)
		if err != nil {
			fmt.Fprintf(os.Stderr, "error: %v\n", err)
		}
		fmt.Println(core.Display(value))
	}
}

func readReplData() ([]core.Datum, error) {
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
	return eval.EvaluateTopLevelProgram(env, topLevelProgram)
}
