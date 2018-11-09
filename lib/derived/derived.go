package derived

import "github.com/katsuya94/grime/runtime"

var Library *runtime.Library

func init() {
	Library = runtime.MustNewLibraryFromFile("derived")
}
