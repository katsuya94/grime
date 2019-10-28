package runtime_test

import (
	"testing"

	"github.com/katsuya94/grime/lib/r6rs"
	"github.com/katsuya94/grime/lib/r6rs_base"
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
			(import (rnrs base))
			(write "hello world")
			`,
			"",
		},
		// {
		// 	"hello world from another library",
		// 	[]string{
		// 		`
		// 		(library (test)
		// 		  (export hello-world)
		// 		  (import (core))
		// 		  (~define hello-world (lambda ()
		// 		    (write "hello world"))))
		// 		`,
		// 	},
		// 	`
		// 	(import (test))
		// 	(hello-world)
		// 	`,
		// 	"",
		// },
		// {
		// 	"hello world export renamed",
		// 	[]string{
		// 		`
		// 		(library (test)
		// 		  (export (rename (hello-world greeting)))
		// 		  (import (core))
		// 		  (~define hello-world (lambda ()
		// 		    (write "hello world"))))
		// 		`,
		// 	},
		// 	`
		// 	(import (test))
		// 	(greeting)
		// 	`,
		// 	"",
		// },
		// {
		// 	"hello world export renamed doesn't leak internal name",
		// 	[]string{
		// 		`
		// 		(library (test)
		// 		  (export (rename (hello-world greeting)))
		// 		  (import (core))
		// 		  (~define hello-world (lambda ()
		// 		    (write "hello world"))))
		// 		`,
		// 	},
		// 	`
		// 	(import (test))
		// 	(hello-world)
		// 	`,
		// 	"runtime: in body expression expanded from string:3:4: compile: unbound identifier hello-world at string:3:5",
		// },
		// {
		// 	"hello world import renamed",
		// 	[]string{
		// 		`
		// 		(library (test)
		// 		  (export hello-world)
		// 		  (import (core))
		// 		  (~define hello-world (lambda ()
		// 		    (write "hello world"))))
		// 		`,
		// 	},
		// 	`
		// 	(import (rename (test) (hello-world greeting)))
		// 	(greeting)
		// 	`,
		// 	"",
		// },
		// {
		// 	"hello world import renamed doesn't leak external name",
		// 	[]string{
		// 		`
		// 		(library (test)
		// 		  (export hello-world)
		// 		  (import (core))
		// 		  (~define hello-world (lambda ()
		// 		    (write "hello world"))))
		// 		`,
		// 	},
		// 	`
		// 	(import (rename (test) (hello-world greeting)))
		// 	(hello-world)
		// 	`,
		// 	"runtime: in body expression expanded from string:3:4: compile: unbound identifier hello-world at string:3:5",
		// },
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			runtime := NewRuntime(r6rs_base.ExpanderFactory{})
			runtime.Add(r6rs.Library)
			runtime.Add(r6rs_base.Library)
			for _, librarySource := range test.librarySources {
				library, err := NewLibrary(read.MustReadSyntax(librarySource))
				if err != nil {
					t.Fatal(err)
				}
				runtime.Add(library)
			}
			program, nullSourceLocationTree := read.MustReadSyntaxes(test.source)
			err := runtime.Run(program, nullSourceLocationTree)
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
