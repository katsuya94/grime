package core_test

import (
	"testing"

	"github.com/stretchr/testify/require"

	"github.com/katsuya94/grime/common"
	. "github.com/katsuya94/grime/lib/core"
	"github.com/katsuya94/grime/read"
	"github.com/katsuya94/grime/test"
)

func TestCompile(t *testing.T) {
	tests := []struct {
		name   string
		source string
		error  string
	}{
		// {
		// 	"malformed quote",
		// 	"(quote)",
		// 	"quote: bad syntax",
		// },
		{
			"duplicate definitions: define, define",
			"(~define foo 'id) (~define foo 'thing)",
			"already defined: foo",
		},
		// {
		// 	"duplicate definitions: define, define-syntax",
		// 	"(~define foo 'id) (define-syntax foo (lambda (x) #''thing))",
		// 	"already defined: foo",
		// },
		// {
		// 	"duplicate definitions: define-syntax, define",
		// 	"(define-syntax foo (lambda (x) #''thing))  (~define foo 'thing)",
		// 	"already defined: foo",
		// },
		// {
		// 	"duplicate definitions: define-syntax, define-syntax",
		// 	"(define-syntax foo (lambda (x) #''thing)) (define-syntax foo (lambda (x) #''thing))",
		// 	"already defined: foo",
		// },
		// {
		// 	"define-syntax",
		// 	"(define-syntax id (lambda (stx) #'123)) (id)",
		// 	"",
		// },
		// {
		// 	"allows nested definitions to shadow",
		// 	"(~define id 'foo) (~let (name 'bar) (~define id 'baz) id)",
		// 	"",
		// },
		// {
		// 	"cannot expand underscore",
		// 	"_",
		// 	"cannot expand underscore",
		// },
		// {
		// 	"cannot expand ellipsis",
		// 	"...",
		// 	"cannot expand ellipsis",
		// },
		// {
		// 	"ellipsis outside pair",
		// 	"(syntax-case #'foo () (_ #'...))",
		// 	"in body expression expanded from string:1:1: compile: in syntax template at string:1:28: improper use of ellipsis",
		// },
		// {
		// 	"ellipsis in first position",
		// 	"(syntax-case #'foo () (_ #'(...)))",
		// 	"in body expression expanded from string:1:1: compile: in syntax template at string:1:29: improper use of ellipsis",
		// },
		// {
		// 	"ellipsis in rest position",
		// 	"(syntax-case #'foo () (id #'(id . ...)))",
		// 	"in body expression expanded from string:1:1: compile: in syntax template at string:1:35: improper use of ellipsis",
		// },
		// {
		// 	"not enough ellipsis",
		// 	"(syntax-case #'((foo)) () (((id ...) ...) #'(id ...)))",
		// 	"in body expression expanded from string:1:1: compile: in syntax template at string:1:45: encountered unexpanded pattern variable",
		// },
		// {
		// 	"not enough ellipsis nested",
		// 	"(syntax-case #'((foo)) () (((id ...) ...) #'((id) ...)))",
		// 	"in body expression expanded from string:1:1: compile: in syntax template at string:1:45: encountered unexpanded pattern variable",
		// },
		// {
		// 	"no pattern variable",
		// 	"(syntax-case #'(foo) () ((_ ...) #'(bar ...)))",
		// 	"in body expression expanded from string:1:1: compile: in syntax template at string:1:37: syntax subtemplate must contain a pattern variable",
		// },
		// {
		// 	"no pattern variable determining expansion count",
		// 	"(syntax-case #'(foo) () ((id ...) #'(id ... ...)))",
		// 	"in body expression expanded from string:1:1: compile: in syntax template at string:1:38: syntax subtemplate must contain a pattern variable determining expansion count",
		// },
	}
	for _, _test := range tests {
		t.Run(_test.name, func(t *testing.T) {
			syntaxes, nullSourceLocationTree := read.MustReadSyntaxes(_test.source)
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
			scopes[1].Set(test.Identifier("lambda"), Bindings[0][common.Symbol("lambda")])
			scopes[1].Set(test.Identifier("syntax"), Bindings[0][common.Symbol("syntax")])
			for phase, scope := range scopes {
				body = body.Push(scope, phase)
			}
			_, err := Compile(body, scopes[0])
			if _test.error == "" {
				require.NoError(t, err)
			} else {
				require.EqualError(t, err, _test.error)
			}
		})
	}
}
