package main

import (
	"fmt"
	"io"
	"syscall/js"

	"github.com/katsuya94/grime/lib/core"
	"github.com/katsuya94/grime/repl"
	"golang.org/x/crypto/ssh/terminal"
)

type Xtermjs struct {
	object   js.Value
	received chan js.Value
}

func NewXtermjs() *Xtermjs {
	object := js.Global().Get("terminal")
	terminal := &Xtermjs{object, make(chan js.Value)}
	object.Set("notify", js.NewCallback(terminal.notify))
	return terminal
}

func (terminal *Xtermjs) notify(args []js.Value) {
	terminal.received <- args[0]
}

func (terminal *Xtermjs) Read(p []byte) (int, error) {
	terminal.object.Call("read", len(p))
	value := <-terminal.received
	n := value.Length()
	for i := 0; i < n; i++ {
		p[i] = byte(value.Index(i).Int())
	}
	return n, nil
}

func (terminal *Xtermjs) Write(p []byte) (int, error) {
	terminal.object.Call("write", string(p))
	return len(p), nil
}

func main() {
	terminal := terminal.NewTerminal(NewXtermjs(), "")
	r, w := io.Pipe()
	go func() {
		for {
			line, err := terminal.ReadLine()
			if err == nil {
				fmt.Fprintln(w, line)
			} else if err == io.EOF {
				fmt.Fprint(w, line)
				w.Close()
				break
			} else {
				panic(err)
			}
		}
	}()
	repl.REPL(core.Bindings, r, terminal)
}
