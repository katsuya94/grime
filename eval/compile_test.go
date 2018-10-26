package eval_test

import (
	"testing"

	"github.com/katsuya94/grime/common"
	. "github.com/katsuya94/grime/eval"
	"github.com/katsuya94/grime/lib/base"
	"github.com/katsuya94/grime/lib/core"
	"github.com/katsuya94/grime/read"
	"github.com/katsuya94/grime/runtime"
)

func TestCompileBody(t *testing.T) {
	tests := []struct {
		name   string
		source string
		error  string
	}{
		{
			"malformed quote",
			"(quote)",
			"quote: bad syntax",
		},
		{
			"duplicate definitions: define, define",
			"(define foo 'id) (define foo 'thing)",
			"previously defined: foo",
		},
		{
			"duplicate definitions: define, define-syntax",
			"(define foo 'id) (define-syntax foo (lambda (x) #''thing))",
			"previously defined: foo",
		},
		{
			"duplicate definitions: define-syntax, define",
			"(define-syntax foo (lambda (x) #''thing)) (define foo 'thing)",
			"previously defined: foo",
		},
		{
			"duplicate definitions: define-syntax, define-syntax",
			"(define-syntax foo (lambda (x) #''thing)) (define-syntax foo (lambda (x) #''thing))",
			"previously defined: foo",
		},
		{
			"empty begin in definition context",
			"(begin) 'foo",
			"",
		},
		{
			"body forms after expression in begin",
			"(begin 'foo (define x 'bar))",
			"compile: unexpected body form in expression context",
		},
		{
			"empty let*",
			"(let* ())",
			"compile: no expressions in body",
		},
		{
			"empty lambda",
			"(lambda ())",
			"compile: no expressions in body",
		},
		{
			"empty define procedure",
			"(define (id)) 'foo",
			"compile: no expressions in body",
		},
		{
			"lambda does not leak enclosing context",
			"((let* ((x 'foo)) (lambda () x))) x",
			"compile: unbound identifier x",
		},
		{
			"lambda does not leak arguments",
			"((lambda (x) x) 'foo) x",
			"compile: unbound identifier x",
		},
		{
			"define-syntax",
			"(define-syntax id (lambda (stx) #''foo)) (id)",
			"",
		},
		{
			"allows nested definitions to shadow",
			"(define id 'foo) (let* () (define id 'bar) id)",
			"",
		},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			rt := runtime.NewRuntime()
			rt.Provide(core.Library)
			rt.Bind(core.Library.Name(), core.Bindings)
			rt.Provide(base.Library)
			bindings, err := rt.BindingsFor([]common.Symbol{common.Symbol("base")})
			if err != nil {
				t.Fatal(err)
			}
			environment := common.NewEnvironment(bindings)
			sourceBody, err := read.ReadString(test.source)
			if err != nil {
				t.Fatal(err)
			}
			var forms []common.Form
			for _, d := range sourceBody {
				forms = append(forms, common.NewWrappedSyntax(d))
			}
			_, _, err = CompileBody(environment, forms)
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
