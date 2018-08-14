package repl

import (
	"github.com/katsuya94/grime/core"
	"github.com/katsuya94/grime/read"
	"os"
	"os/signal"
)

func REPL() {
	reader := NewREPLDatumReader()
	reader.Start(os.Stdin)
}

func (r *REPL) interrupt() {
	c := make(chan os.Signal)
	signal.Notify(c, os.Interrupt)
	go func() {
		<- c
		r.c <- interruptEvent{}
	}()
}


