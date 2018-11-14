package main

import (
	"fmt"
	"os"

	"golang.org/x/tools/go/packages"
)

func main() {
	packs, err := packages.Load(&packages.Config{Mode: packages.LoadTypes}, "fmt")
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
	}
	fmt.Printf("%#v\n", packs[0].Types.Scope().Lookup("Print"))
}
