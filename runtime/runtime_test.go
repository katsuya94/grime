package runtime_test

import (
	"testing"

	"github.com/katsuya94/grime/lib/core"
	"github.com/katsuya94/grime/read"
	. "github.com/katsuya94/grime/runtime"
)

func TestRuntime_Execute(t *testing.T) {
	tests := []struct {
		name   string
		source string
		error  string
	}{
		{
			"hello world",
			`
			(import (core))
			(write "hello world")
			`,
			"",
		},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			data, err := read.ReadString(test.source)
			if err != nil {
				t.Fatal(err)
			}
			runtime := NewRuntime()
			runtime.Provide(core.Library)
			runtime.Bind(core.Library.Name(), core.Bindings)
			err = runtime.Execute(data)
			if test.error != "" {
				if err == nil || err.Error() != test.error {
					t.Fatalf("\nexpected error: %v\n     got error: %v\n", test.error, err)
				}
			} else if err != nil {
				t.Fatal(err)
			}
		})
	}
}
