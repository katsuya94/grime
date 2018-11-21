package main

import (
	"fmt"
	"io"
	"syscall/js"

	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/lib/base"
	"github.com/katsuya94/grime/lib/core"
	"github.com/katsuya94/grime/lib/derived"
	"github.com/katsuya94/grime/lib/grime"
	"github.com/katsuya94/grime/runtime"
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
	rt := runtime.NewRuntime(core.Compile)
	rt.MustProvide(core.Library)
	rt.MustBind(core.Library.Name(), core.Bindings)
	rt.MustProvide(derived.Library)
	rt.MustProvide(base.Library)
	rt.MustProvide(grime.Library)
	rt.MustInstantiate([]common.Symbol{common.Symbol("grime")})
	bindings, err := rt.BindingsFor([]common.Symbol{common.Symbol("grime")})
	if err != nil {
		panic(err)
	}
	runtime.REPL(core.Compile, bindings, r, terminal)
}
