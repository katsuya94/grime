package base

import (
	"fmt"
	"os"
	"path/filepath"
	"runtime"

	"github.com/katsuya94/grime/read"
	grime "github.com/katsuya94/grime/runtime"
)

var Library *grime.Library

func init() {
	_, filename, _, ok := runtime.Caller(0)
	if !ok {
		panic("failed to load base")
	}
	sourcePath := filepath.Join(filepath.Dir(filename), "base.scm")
	f, err := os.Open(sourcePath)
	if err != nil {
		panic(fmt.Sprintf("failed to load base: %v", err))
	}
	data := read.MustRead(f)
	if len(data) != 1 {
		panic(fmt.Sprintf("failed to load base: found %v data", len(data)))
	}
	Library = grime.MustNewLibrary(data[0])
}
