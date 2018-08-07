package main

import (
	"fmt"
	"github.com/katsuya94/grime/interpret"
	"github.com/katsuya94/grime/read"
	"io"
	"os"
)

func main() {
	r := read.NewDatumReader(os.Stdin)
	env := interpret.NewEnvironment()
	for {
		fmt.Print("grime> ")
		datum, err := r.ReadDatum()
		if err == io.EOF {
			break
		} else if err != nil {
			fmt.Fprint(os.Stderr, err)
		}
		value, err := env.Evaluate(datum)
		if err != nil {
			fmt.Fprint(os.Stderr, err)
		}
		fmt.Printf("%#v\n", value)
	}
}
