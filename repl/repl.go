package repl

import (
	"bufio"
	"bytes"
	"fmt"
	"github.com/katsuya94/grime/core"
	"github.com/katsuya94/grime/eval"
	"github.com/katsuya94/grime/read"
	"io"
	"os"
	"os/signal"
)

type replEvent interface{}

type datumEvent struct {
	id    uint64
	datum core.Datum
}
type interruptEvent struct{}
type recoverableErrorEvent struct {
	id  uint64
	err error
}
type unrecoverableErrorEvent struct{ err error }

type REPL struct {
	reader  io.Reader
	writer  io.WriteCloser
	scanner *bufio.Scanner
	c       chan replEvent
	env     *eval.Environment
}

func NewREPL() *REPL {
	scanner := bufio.NewScanner(os.Stdin)
	scanner.Split(splitReplLines)
	pipeReader, pipeWriter := io.Pipe()
	return &REPL{
		pipeReader,
		pipeWriter,
		scanner,
		make(chan replEvent),
		eval.NewEnvironment(),
	}
}

func (r *REPL) Run() error {
	r.interrupt()
	r.scan()
	id := r.read(0)
	for {
		fmt.Print("grime> ")
		e := <-r.c
		fmt.Printf("%#v", e)
		switch v := e.(type) {
		case datumEvent:
			if v.id == id {
				fmt.Println(core.Display(v.datum))
			}
		case interruptEvent:
			fmt.Println("")
			id = r.read(id)
		case recoverableErrorEvent:
			if err := v.err; v.id == id {
				if err == io.EOF {
					return nil
				} else if err != nil {
					fmt.Fprintf(os.Stderr, "error: %v\n", err)
					id = r.read(id)
				} else {
					return fmt.Errorf("repl: received recoverable error event with no error")
				}
			}
		case unrecoverableErrorEvent:
			if err := v.err; err == io.EOF {
				return nil
			} else if err != nil {
				return fmt.Errorf("repl: %v", err)
			} else {
				return fmt.Errorf("repl: received unrecoverable error event with no error")
			}
		}
	}
}

func (r *REPL) scan() {
	go func() {
		for {
			if r.scanner.Scan() {
				r.writer.Write(r.scanner.Bytes())
			} else if err := r.scanner.Err(); err != nil {
				r.c <- unrecoverableErrorEvent{err}
				return
			} else {
				r.writer.Close()
				return
			}
		}
	}()
}

func (r *REPL) read(last uint64) uint64 {
	id := last + 1
	go func() {
		datumReader := read.NewDatumReader(r.reader)
		for {
			if datum, err := datumReader.ReadDatum(); err != nil {
				r.c <- recoverableErrorEvent{id, err}
				return
			} else {
				r.c <- datumEvent{id, datum}
			}
		}
	}()
	return id
}

func (r *REPL) interrupt() {
	c := make(chan os.Signal)
	signal.Notify(c, os.Interrupt)
	go func() {
		<- c
		r.c <- interruptEvent{}
	}()
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
