package core_test

import (
	"reflect"
	"testing"

	"github.com/katsuya94/grime/common"
	. "github.com/katsuya94/grime/lib/core"
	"github.com/katsuya94/grime/read"
	"github.com/stretchr/testify/require"
)

func TestCore(t *testing.T) {
	tests := []struct {
		name   string
		source string
		val    string
		error  string
	}{
		{
			"boolean literal",
			"#f",
			"#f",
			"",
		},
		{
			"number literal",
			"123",
			"123",
			"",
		},
		{
			"character literal",
			`#\x`,
			`#\x`,
			"",
		},
		{
			"string literal",
			`"name"`,
			`"name"`,
			"",
		},
		{
			"symbol literal",
			"'id",
			"id",
			"",
		},
		{
			"list literal",
			"'(id)",
			"(id)",
			"",
		},
		{
			"if then",
			"(if #t 'foo 'bar)",
			"foo",
			"",
		},
		{
			"if else",
			"(if #f 'foo 'bar)",
			"bar",
			"",
		},
		{
			"if non-boolean",
			"(if 'id 'foo 'bar)",
			"foo",
			"",
		},
		{
			"let",
			"(~let (x 'id) x)",
			"id",
			"",
		},
		{
			"define",
			"(~define x 'foo) x",
			"foo",
			"",
		},
		{
			"begin in splicing context",
			"(begin 'foo 'bar)",
			"bar",
			"",
		},
		{
			"begin in expression context",
			"(cons (begin 'foo 'bar) '())",
			"(bar)",
			"",
		},
		{
			"begin with one subform",
			"(begin 'foo)",
			"foo",
			"",
		},
		{
			"begin with multiple subforms",
			"(begin 'bar 'foo)",
			"foo",
			"",
		},
		{
			"begin with body forms",
			"(begin (~define x 'foo) x)",
			"foo",
			"",
		},
		{
			"lambda",
			"((lambda () 'foo))",
			"foo",
			"",
		},
		{
			"lambda with argument",
			"((lambda (x) x) 'foo)",
			"foo",
			"",
		},
		{
			"lambda using enclosing context",
			"((~let (x 'foo) (lambda () x)))",
			"foo",
			"",
		},
		{
			"call/cc used to escape early",
			"(call/cc (lambda (c) (c 'foo) 'bar))",
			"foo",
			"",
		},
		{
			"set! used to set a defined variable",
			"(~define x 'bar) (set! x 'foo) x",
			"foo",
			"",
		},
		{
			"call/cc and set! used to loop",
			`
			(~define in '(foo bar baz))
			(~define continue #f)
			(~define out (call/cc (lambda (c) (set! continue c) '())))
			(if (null? in)
				out
				(begin
					(~define new (cons (car in) out))
					(set! in (cdr in))
					(continue new)))
			`,
			"(baz bar foo)",
			"",
		},
		{
			"error raises errors",
			`(error "well that's too bad")`,
			"",
			"well that's too bad",
		},
		{
			"cannot reference identifer before its definition",
			"(~define foo (lambda () bar)) (~define baz (foo)) (~define bar 'id) baz",
			"",
			"evaluate: bar at string:1:25: cannot reference identifier before its definition",
		},
		{
			"cannot set identifer before its definition",
			"(~define foo (lambda () (set! bar 'thing))) (~define baz (foo)) (~define bar 'id) baz",
			"",
			"evaluate: bar at string:1:31: cannot set identifier before its definition",
		},
		{
			"define-syntax",
			"(define-syntax id (lambda (stx) #'123)) (id)",
			"123",
			"",
		},
		{
			"syntax-case wildcard",
			"(syntax->datum (syntax-case #'bar () (_ #'foo)))",
			"foo",
			"",
		},
		{
			"syntax-case capture",
			"(syntax->datum (syntax-case #'foo () (id #'id)))",
			"foo",
			"",
		},
		{
			"syntax-case ellipsis",
			"(syntax->datum (syntax-case #'(foo bar) () ((id ...) #'(id ...))))",
			"(foo bar)",
			"",
		},
		{
			"syntax-case nested ellipsis",
			"(syntax->datum (syntax-case #'((foo) (bar baz)) () (((id ...) ...) #'((id ...) ...))))",
			"((foo) (bar baz))",
			"",
		},
		{
			"syntax-case nested ellipsis flattened",
			"(syntax->datum (syntax-case #'((foo) (bar baz)) () (((id ...) ...) #'(id ... ...))))",
			"(foo bar baz)",
			"",
		},
		{
			"syntax-case ellipsis repeated",
			"(syntax->datum (syntax-case #'((foo bar) (baz qux)) () (((id ...) (thing ...)) #'((id thing ...) ...))))",
			"((foo baz qux) (bar baz qux))",
			"",
		},
		{
			"syntax-case failure",
			"(syntax-case #'bar () ((id) #'foo))",
			"",
			"in macro use at string:1:16: bad syntax",
		},
		{
			"syntax-case fender",
			"(syntax-case #'bar () (_ #f #'foo))",
			"",
			"in macro use at string:1:16: bad syntax",
		},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			var expected common.Datum
			if test.val != "" {
				expected = read.MustReadDatum(test.val)
			}
			syntaxes, nullSourceLocationTree := read.MustReadSyntaxes(test.source)
			body := common.Body(nullSourceLocationTree, syntaxes...)
			scopeSet := common.NewScopeSet()
			frame := common.NewFrame()
			err := Bindings.Load([]int{0}, scopeSet, frame, common.IdentifierTransformerFactoryAll)
			require.NoError(t, err)
			scopeSet.Set(1, common.Symbol("lambda"), Bindings.Get(common.Symbol("lambda"), 0))
			scopeSet.Set(1, common.Symbol("syntax"), Bindings.Get(common.Symbol("syntax"), 0))
			body = scopeSet.Apply(body)
			frameTemplate := frame.Template()
			stack := common.NewStack(frame)
			var expression common.Expression
			require.NotPanics(t, func() {
				expression, err = NewCompiler().Compile(body, scopeSet[0], &frameTemplate, stack)
			})
			require.NoError(t, err)
			frame.Grow(frameTemplate)
			var actual common.Datum
			require.NotPanics(t, func() {
				actual, err = common.Evaluate(stack, expression)
			})
			if test.error != "" {
				if err == nil || err.Error() != test.error {
					t.Fatalf("\nexpected error: %v\n     got error: %v\n", test.error, err)
				}
			} else if err != nil {
				t.Fatal(err)
			} else if !reflect.DeepEqual(actual, expected) {
				t.Fatalf("\nexpected: %#v\n     got: %#v", expected, actual)
			}
		})
	}
}

func TestDefineSyntaxReferenceGlobal(t *testing.T) {
	source := `
(define-syntax reference-global
	(lambda (stx)
		#'(cons 'foo 'bar)))
(reference-global)
`
	testProgram(t, source, read.MustReadDatum("(foo . bar)"))
}

func TestDefineSyntaxReferenceLocal(t *testing.T) {
	source := `
(~define id 'foo)
(define-syntax reference-local
	(lambda (stx)
		(~define id 'bar)
		; TODO: it's looking like we need to reevaluate the scopes within which we resolve transformer output
		#'id))
(reference-local)
`
	testProgram(t, source, read.MustReadDatum("foo"))
}

func TestSyntaxCaseHygiene(t *testing.T) {
	source := `
(define-syntax with-marked-id
	(lambda (stx)
		(syntax-case stx ()
			[(_ e) #'(~let (id #f) e)])))
(~let (id #t) (with-marked-id id))
`
	testProgram(t, source, read.MustReadDatum("#t"))
}

func TestSyntaxCaseLambdaHygiene(t *testing.T) {
	source := `
(define-syntax marked-id
	(lambda (stx)
		(syntax-case stx ()
			[(_) #'#'id])))
(define-syntax syntax-case-with-unmarked-and-marked-lambda-ids
	(lambda (stx)
		(syntax-case (cons #'id (marked-id)) ()
			[(unmarked . marked)
				#'(lambda (unmarked marked)
						; TODO: this reference to cons references frame 2, but len(stack) is 2
						; the frame introduced by the transformer during expansion is not introduced during evaluation
						(cons unmarked marked))])))
((syntax-case-with-unmarked-and-marked-lambda-ids) 'foo 'bar)
`
	testProgram(t, source, read.MustReadDatum("(foo . bar)"))
}

func TestLambdaReentrance(t *testing.T) {
	source := `
(~define append (lambda (left right)
	(if (null? left)
		right
		(begin
			(~define rest (append (cdr left) right))
			(cons (car left) rest)))))
(append '(a b c) '(d e f))
`
	testProgram(t, source, read.MustReadDatum("(a b c d e f)"))
}

func TestLambdaClosure(t *testing.T) {
	source := `
(~define make-add (lambda (f)
	(lambda (r)
		(cons f r))))
(~define add-foo (make-add 'foo))
(~define add-bar (make-add 'bar))
(add-foo (add-bar '(baz)))
`
	testProgram(t, source, read.MustReadDatum("(foo bar baz)"))
}
