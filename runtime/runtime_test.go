package runtime_test

import (
	"testing"

	"github.com/katsuya94/grime/read"
)

func TestImplementation(t *testing.T) {
	implementation := common.NewImplementation()
	implementation.Load(Core)
	data, err := read.ReadString(`
`)
	if err != nil {
		t.Fatal(err)
	}
	implementation.RunTopLevelProgram(data)
}
