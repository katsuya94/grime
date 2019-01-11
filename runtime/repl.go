package runtime

import (
	"bufio"
	"bytes"
	"fmt"
	"io"

	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/read"
)

func REPL(compiler common.Compiler, bindings common.BindingSet, r io.Reader, w io.Writer) {
	var forms []common.Datum
	for {
		fmt.Fprintf(w, "grime:%v> ", len(forms))
		eof := false
		syntaxes, nullSourceLocationTree, err := readREPLSyntaxes(r)
		if err == io.EOF {
			if len(forms) == 0 {
				break
			} else {
				eof = true
			}
		} else if err != nil {
			forms = nil
			fmt.Fprintf(w, "error: %v\n", err)
			continue
		}
		body := common.Body(nullSourceLocationTree, syntaxes...)
		scopes := make(map[int]common.BaseScope, len(bindings))
		for phase, locations := range bindings {
			scopes[phase] = common.NewScope()
			for name, location := range locations {
				err := scopes[phase].Set(common.NewIdentifier(name), location)
				if err != nil {
					fmt.Fprintf(w, "error: %v\n", err)
					continue
				}
			}
			body = body.Push(scopes[phase], phase)
		}
		expression, err := compiler(body, scopes[0])
		if err == common.ErrUnexpectedFinalForm && !eof {
			continue
		}
		forms = nil
		if err != nil {
			fmt.Fprintf(w, "error: %v\n", err)
			continue
		}
		result, err := common.EvaluateOnce(expression)
		if err != nil {
			fmt.Fprintf(w, "error: %v\n", err)
			continue
		}
		bindings = make(common.BindingSet, len(bindings))
		for phase, scope := range scopes {
			bindings[phase] = make(map[common.Symbol]common.Location)
			for name, location := range scope.Bindings() {
				bindings[phase][name] = location
			}
		}
		fmt.Fprintln(w, common.Write(result))
	}
}

func readREPLSyntaxes(r io.Reader) ([]common.Syntax, common.SourceLocationTree, error) {
	var source []byte
	scanner := bufio.NewScanner(r)
	scanner.Split(splitReplLines)
	for {
		if scanner.Scan() {
			source = append(source, scanner.Bytes()...)
			if syntaxes, nullSourceLocationTree, err := read.ReadBytes(source); err == nil {
				return syntaxes, nullSourceLocationTree, nil
			} else if _, ok := err.(read.UnexpectedEOFError); !ok {
				return nil, common.SourceLocationTree{}, err
			}
		} else if err := scanner.Err(); err != nil {
			return nil, common.SourceLocationTree{}, err
		} else {
			source = append(source, scanner.Bytes()...)
			if syntaxes, nullSourceLocationTree, err := read.ReadBytes(source); syntaxes == nil && err == nil {
				return nil, nullSourceLocationTree, io.EOF
			} else {
				return syntaxes, common.SourceLocationTree{}, err
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
