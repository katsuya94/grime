package runtime

import (
	"bufio"
	"bytes"
	"fmt"
	"io"

	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/read"
	"github.com/katsuya94/grime/util"
)

func REPL(compiler common.Compiler, bindings common.BindingSet, r io.Reader, w io.Writer) {
	var forms []common.Datum
	for {
		fmt.Fprintf(w, "grime:%v> ", len(forms))
		eof := false
		data, err := readREPLData(r)
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
		body := common.NewWrappedSyntax(util.List(data...))
		for phase, locations := range bindings {
			for name, location := range locations {
				body = body.SetAt(name, phase, location)
			}
		}
		expression, definitions, err := compiler(body)
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
		for _, definition := range definitions {
			for _, phase := range definition.Phases() {
				_, ok := bindings[phase]
				if !ok {
					bindings[phase] = make(map[common.Symbol]common.Location)
				}
				name, location := definition.IdentifierAt(phase)
				bindings[phase][name] = location
			}
		}
		fmt.Fprintln(w, common.Write(result))
	}
}

func readREPLData(r io.Reader) ([]common.Datum, error) {
	var source []byte
	scanner := bufio.NewScanner(r)
	scanner.Split(splitReplLines)
	for {
		if scanner.Scan() {
			source = append(source, scanner.Bytes()...)
			if data, _, err := read.ReadBytes(source); err == nil {
				return data, nil
			} else if _, ok := err.(read.UnexpectedEOFError); !ok {
				return nil, err
			}
		} else if err := scanner.Err(); err != nil {
			return nil, err
		} else {
			source = append(source, scanner.Bytes()...)
			if data, _, err := read.ReadBytes(source); data == nil && err == nil {
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
