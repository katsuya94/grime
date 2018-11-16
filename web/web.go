package main

import (
	"bufio"
	"bytes"
	"fmt"
	"io"
	"syscall/js"

	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/eval"
	"github.com/katsuya94/grime/lib/core"
	"github.com/katsuya94/grime/read"
)

type DOMTerminal struct {
	element js.Value
}

func NewDOMTerminal() *DOMTerminal {
	element := js.Global().Get("document").Call("getElementById", "terminal")
	return &DOMTerminal{element}
}

func (terminal *DOMTerminal) Read(p []byte) (int, error) {
	panic("HERE")
	return 0, nil
}

func (terminal *DOMTerminal) Write(p []byte) (int, error) {
	terminal.element.Call("append", string(p))
	return len(p), nil
}

func main() {
	err := run()
	if err != nil {
		panic(err)
	}
}

func run() error {
	terminal := NewDOMTerminal()
	REPL(core.Bindings, terminal)
	return nil
}

func REPL(bindings common.BindingSet, terminal io.ReadWriter) {
	var forms []common.Form
	for {
		fmt.Fprintf(terminal, "grime:%v> ", len(forms))
		eof := false
		data, err := readREPLData(terminal)
		if err == io.EOF {
			if len(forms) == 0 {
				break
			} else {
				eof = true
			}
		} else if err != nil {
			forms = nil
			fmt.Fprintf(terminal, "error: %v\n", err)
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
			fmt.Fprintf(terminal, "error: %v\n", err)
			continue
		}
		result, err := eval.EvaluateExpressionOnce(expression)
		if err != nil {
			fmt.Fprintf(terminal, "error: %v\n", err)
			continue
		}
		bindings = newBindings
		fmt.Fprintln(terminal, common.Write(result))
	}
}

func readREPLData(terminal io.Reader) ([]common.Datum, error) {
	var source []byte
	scanner := bufio.NewScanner(terminal)
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
