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
			"already defined: foo",
		},
		{
			"duplicate definitions: define, define-syntax",
			"(~define foo 'id) (define-syntax foo (lambda (x) #''thing))",
			"already defined: foo",
		},
		{
			"duplicate definitions: define-syntax, define",
			"(define-syntax foo (lambda (x) #''thing))  (~define foo 'thing)",
			"already defined: foo",
		},
		{
			"duplicate definitions: define-syntax, define-syntax",
			"(define-syntax foo (lambda (x) #''thing)) (define-syntax foo (lambda (x) #''thing))",
			"already defined: foo",
		},
		{
			"empty begin in definition context",
			"(begin) 'foo",
			"",
		},
		{
			"body forms after expression in begin",
			"(begin 'foo (~define x 'bar))",
			"in body expression expanded from string:1:1: in body expression expanded from string:1:13: compile: unexpected form in expression context: #<core.DefineForm>",
		},
		{
			"empty lambda",
			"(lambda ())",
			"in body expression expanded from string:1:1: unexpected final form",
		},
		{
			"lambda does not leak enclosing context",
			"((~let (x 'foo) (lambda () x))) x",
			"in body expression expanded from string:1:33: compile: unbound identifier x at string:1:33",
		},
		{
			"lambda does not leak arguments",
			"((lambda (x) x) 'foo) x",
			"in body expression expanded from string:1:23: compile: unbound identifier x at string:1:23",
		},
		{
			"define-syntax",
			"(define-syntax id (lambda (stx) #'123)) (id)",
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
			"in body expression expanded from string:1:1: compile: improper use of ellipsis in syntax template",
		},
		{
			"ellipsis in first position",
			"(syntax-case #'foo () (_ #'(...)))",
			"in body expression expanded from string:1:1: compile: improper use of ellipsis in syntax template",
		},
		{
			"ellipsis in rest position",
			"(syntax-case #'foo () (id #'(id . ...)))",
			"in body expression expanded from string:1:1: compile: improper use of ellipsis in syntax template",
		},
		{
			"not enough ellipsis",
			"(syntax-case #'((foo)) () (((id ...) ...) #'(id ...)))",
			"in body expression expanded from string:1:1: compile: encountered unexpanded pattern variable",
		},
		{
			"not enough ellipsis nested",
			"(syntax-case #'((foo)) () (((id ...) ...) #'((id) ...)))",
			"in body expression expanded from string:1:1: compile: encountered unexpanded pattern variable",
		},
		{
			"no pattern variable",
			"(syntax-case #'(foo) () ((_ ...) #'(bar ...)))",
			"in body expression expanded from string:1:1: compile: syntax subtemplate must contain a pattern variable",
		},
		{
			"no pattern variable determining expansion count",
			"(syntax-case #'(foo) () ((id ...) #'(id ... ...)))",
			"in body expression expanded from string:1:1: compile: syntax subtemplate must contain a pattern variable determining expansion count",
		},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			syntaxes, nullSourceLocationTree := read.MustReadSyntaxes(test.source)
			body := common.Body(nullSourceLocationTree, syntaxes...)
			scopes := make(map[int]common.Scope)
			for phase, locations := range Bindings {
				scopes[phase] = common.NewScope()
				for name, location := range locations {
					scopes[phase].Set(common.NewIdentifier(name), location)
				}
			}
			// Make lambda, syntax available at phase 1
			if _, ok := scopes[1]; !ok {
				scopes[1] = common.NewScope()
			}
			scopes[1].Set(common.NewIdentifier(common.Symbol("lambda")), Bindings[0][common.Symbol("lambda")])
			scopes[1].Set(common.NewIdentifier(common.Symbol("syntax")), Bindings[0][common.Symbol("syntax")])
			for phase, scope := range scopes {
				body = body.Push(scope, phase)
			}
			_, err := Compile(body, scopes[0])
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
