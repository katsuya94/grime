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
				  (define (hello-world)
				    (write "hello world")))
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
				  (define (hello-world)
				    (write "hello world")))
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
				  (define (hello-world)
				    (write "hello world")))
				`,
			},
			`
			(import (test))
			(hello-world)
			`,
			"compile: unbound identifier hello-world",
		},
		{
			"hello world import renamed",
			[]string{
				`
				(library (test)
				  (export hello-world)
				  (import (core))
				  (define (hello-world)
				    (write "hello world")))
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
				  (define (hello-world)
				    (write "hello world")))
				`,
			},
			`
			(import (rename (test) (hello-world greeting)))
			(hello-world)
			`,
			"compile: unbound identifier hello-world",
		},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			runtime := NewRuntime()
			err := runtime.Provide(core.Library)
			if err != nil {
				t.Fatal(err)
			}
			err = runtime.Bind(core.Library.Name(), core.Bindings)
			if err != nil {
				t.Fatal(err)
			}
			for _, librarySource := range test.librarySources {
				data, err := read.ReadString(librarySource)
				if err != nil {
					t.Fatal(err)
				} else if len(data) != 1 {
					t.Fatalf("encountered %v data in pattern", len(data))
				}
				library, err := NewLibrary(data[0])
				if err != nil {
					t.Fatal(err)
				}
				err = runtime.Provide(library)
				if err != nil {
					t.Fatal(err)
				}
			}
			data, err := read.ReadString(test.source)
			if err != nil {
				t.Fatal(err)
			}
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
