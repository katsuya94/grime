package runtime_test

import (
	"testing"

	"github.com/katsuya94/grime/lib/core"
	"github.com/katsuya94/grime/read"
	. "github.com/katsuya94/grime/runtime"
)

func TestRuntime_Execute(t *testing.T) {
	tests := []struct {
		name           string
		librarySources []string
		source         string
		error          string
	}{
		{
			"hello world",
			[]string{},
			`
			(import (core))
			(write "hello world")
			`,
			"",
		},
		{
			"hello world from another library",
			[]string{
				`
				(library (test)
				  (export hello-world)
				  (import (core))
				  (~define hello-world (lambda ()
				    (write "hello world"))))
				`,
			},
			`
			(import (test))
			(hello-world)
			`,
			"",
		},
		{
			"hello world export renamed",
			[]string{
				`
				(library (test)
				  (export (rename (hello-world greeting)))
				  (import (core))
				  (~define hello-world (lambda ()
				    (write "hello world"))))
				`,
			},
			`
			(import (test))
			(greeting)
			`,
			"",
		},
		{
			"hello world export renamed doesn't leak internal name",
			[]string{
				`
				(library (test)
				  (export (rename (hello-world greeting)))
				  (import (core))
				  (~define hello-world (lambda ()
				    (write "hello world"))))
				`,
			},
			`
			(import (test))
			(hello-world)
			`,
			"runtime: compile: unbound identifier hello-world",
		},
		{
			"hello world import renamed",
			[]string{
				`
				(library (test)
				  (export hello-world)
				  (import (core))
				  (~define hello-world (lambda ()
				    (write "hello world"))))
				`,
			},
			`
			(import (rename (test) (hello-world greeting)))
			(greeting)
			`,
			"",
		},
		{
			"hello world import renamed doesn't leak external name",
			[]string{
				`
				(library (test)
				  (export hello-world)
				  (import (core))
				  (~define hello-world (lambda ()
				    (write "hello world"))))
				`,
			},
			`
			(import (rename (test) (hello-world greeting)))
			(hello-world)
			`,
			"runtime: compile: unbound identifier hello-world",
		},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			runtime := NewRuntime(core.Compile)
			runtime.MustProvide(core.Library)
			runtime.MustBind(core.Library.Name(), core.Bindings)
			for _, librarySource := range test.librarySources {
				library, err := NewLibrary(read.MustReadDatum(librarySource))
				if err != nil {
					t.Fatal(err)
				}
				err = runtime.Provide(library)
				if err != nil {
					t.Fatal(err)
				}
			}
			program := read.MustReadData(test.source)
			err := runtime.Execute(program)
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
