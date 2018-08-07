package main

import (
	"fmt"
	"github.com/katsuya94/grime/read"
	"os"
	"io"
)

func main() {
	r := read.NewDatumReader(os.Stdin)
	for {
		fmt.Print("grime> ")
		datum, err := r.ReadDatum()
		if err == io.EOF {
			break
		} else if err != nil {
			fmt.Fprint(os.Stderr, err)
		}
		fmt.Printf("%#v\n", datum)
	}
}

