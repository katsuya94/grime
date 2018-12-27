package core_test

import (
	"testing"

	"github.com/katsuya94/grime/common"
	. "github.com/katsuya94/grime/lib/core"
	"github.com/katsuya94/grime/read"
)

func TestCompile(t *testing.T) {
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
			"(~define foo 'id) (~define foo 'thing)",
			"compile: foo: already defined",
		},
		{
			"duplicate definitions: define, define-syntax",
			"(~define foo 'id) (define-syntax foo (lambda (x) #''thing))",
			"compile: foo: already defined",
		},
		{
			"duplicate definitions: define-syntax, define",
			"(define-syntax foo (lambda (x) #''thing))  (~define foo 'thing)",
			"compile: foo: already defined",
		},
		{
			"duplicate definitions: define-syntax, define-syntax",
			"(define-syntax foo (lambda (x) #''thing)) (define-syntax foo (lambda (x) #''thing))",
			"compile: foo: already defined",
		},
		{
			"empty begin in definition context",
			"(begin) 'foo",
			"",
		},
		{
			"body forms after expression in begin",
			"(begin 'foo (~define x 'bar))",
			"compile: unexpected form in expression context: #<core.DefineForm>",
		},
		{
			"empty lambda",
			"(lambda ())",
			"unexpected final form",
		},
		{
			"lambda does not leak enclosing context",
			"((~let (x 'foo) (lambda () x))) x",
			"compile: unbound identifier x at string:1:33",
		},
		{
			"lambda does not leak arguments",
			"((lambda (x) x) 'foo) x",
			"compile: unbound identifier x at string:1:23",
		},
		{
			"define-syntax",
			"(define-syntax id (lambda (stx) #''foo)) (id)",
			"",
		},
		{
			"allows nested definitions to shadow",
			"(~define id 'foo) (~let (name 'bar) (~define id 'baz) id)",
			"",
		},
		{
			"cannot expand underscore",
			"_",
			"cannot expand underscore",
		},
		{
			"cannot expand ellipsis",
			"...",
			"cannot expand ellipsis",
		},
		{
			"ellipsis outside pair",
			"(syntax-case #'foo () (_ #'...))",
			"compile: improper use of ellipsis in syntax template",
		},
		{
			"ellipsis in first position",
			"(syntax-case #'foo () (_ #'(...)))",
			"compile: improper use of ellipsis in syntax template",
		},
		{
			"ellipsis in rest position",
			"(syntax-case #'foo () (id #'(id . ...)))",
			"compile: improper use of ellipsis in syntax template",
		},
		{
			"not enough ellipsis",
			"(syntax-case #'((foo)) () (((id ...) ...) #'(id ...)))",
			"compile: encountered unexpanded pattern variable",
		},
		{
			"not enough ellipsis nested",
			"(syntax-case #'((foo)) () (((id ...) ...) #'((id) ...)))",
			"compile: encountered unexpanded pattern variable",
		},
		{
			"no pattern variable",
			"(syntax-case #'(foo) () ((_ ...) #'(bar ...)))",
			"compile: syntax subtemplate must contain a pattern variable",
		},
		{
			"no pattern variable determining expansion count",
			"(syntax-case #'(foo) () ((id ...) #'(id ... ...)))",
			"compile: syntax subtemplate must contain a pattern variable determining expansion count",
		},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			syntaxes, nullSourceLocationTree := read.MustReadSyntaxes(test.source)
			body := common.Body(nullSourceLocationTree, syntaxes...)
			for phase, locations := range Bindings {
				for name, location := range locations {
					body = body.SetAt(name, phase, location)
				}
			}
			// Make lambda, syntax available at phase 1
			body = body.SetAt(common.Symbol("lambda"), 1, Bindings[0][common.Symbol("lambda")])
			body = body.SetAt(common.Symbol("syntax"), 1, Bindings[0][common.Symbol("syntax")])
			_, _, err := Compile(body)
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
