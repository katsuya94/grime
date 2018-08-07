package main

import (
	"fmt"
	"github.com/katsuya94/grime/eval"
	"github.com/katsuya94/grime/read"
	"io"
	"os"
)

func main() {
	var (
		r = read.NewDatumReader(os.Stdin)
		e = eval.NewEnvironment()
	)
	for {
		fmt.Print("grime> ")
		datum, err := r.ReadDatum()
		if err == io.EOF {
			break
		} else if err != nil {
			fmt.Fprintln(os.Stderr, err)
			os.Exit(1)
		}
		value, err := e.Evaluate(datum)
		if err != nil {
			fmt.Fprintln(os.Stderr, err)
			os.Exit(1)
		}
		fmt.Printf("%#v\n", value)
	}
}
