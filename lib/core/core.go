package core

import (
	"fmt"

	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/eval"
	"github.com/katsuya94/grime/read"
	"github.com/katsuya94/grime/runtime"
	"github.com/katsuya94/grime/util"
)

var Library *runtime.Library = runtime.MustNewEmptyLibrary([]common.Symbol{common.Symbol("core")}, []int{})

var Bindings common.BindingSet

func init() {
	env := common.EmptyEnvironment
	env = env.MustDefine(common.Symbol("quote"), []int{0}, &common.Keyword{common.Function(transformQuote)})
	env = env.MustDefine(common.Symbol("syntax"), []int{0}, &common.Keyword{common.Function(transformSyntax)})
	env = env.MustDefine(common.Symbol("if"), []int{0}, &common.Keyword{common.Function(transformIf)})
	env = env.MustDefine(common.Symbol("let*"), []int{0}, &common.Keyword{common.Function(transformLetStar)})
	env = env.MustDefine(common.Symbol("begin"), []int{0}, &common.Keyword{common.Function(transformBegin)})
	env = env.MustDefine(common.Symbol("lambda"), []int{0}, &common.Keyword{common.Function(transformLambda)})
	env = env.MustDefine(common.Symbol("define"), []int{0}, &common.Keyword{common.Function(transformDefine)})
	env = env.MustDefine(common.Symbol("define-syntax"), []int{0}, &common.Keyword{common.Function(transformDefineSyntax)})
	env = env.MustDefine(common.Symbol("syntax-case"), []int{0}, &common.Keyword{common.Function(transformSyntaxCase)})
	env = env.MustDefine(common.Symbol("set!"), []int{0}, &common.Keyword{common.Function(transformSet)})
	env = env.MustDefine(common.Symbol("_"), []int{0}, common.WildcardKeyword)
	env = env.MustDefine(common.Symbol("..."), []int{0}, common.EllipsisKeyword)
	env = env.MustDefine(common.Symbol("cons"), []int{0}, &common.Variable{common.Function(cons), true})
	env = env.MustDefine(common.Symbol("car"), []int{0}, &common.Variable{common.Function(car), true})
	env = env.MustDefine(common.Symbol("cdr"), []int{0}, &common.Variable{common.Function(cdr), true})
	env = env.MustDefine(common.Symbol("null?"), []int{0}, &common.Variable{common.Function(null), true})
	env = env.MustDefine(common.Symbol("write"), []int{0}, &common.Variable{common.Function(write), true})
	env = env.MustDefine(common.Symbol("call/cc"), []int{0}, &common.Variable{common.Function(callWithCurrentContinuation), true})
	env = env.MustDefine(common.Symbol("error"), []int{0}, &common.Variable{common.Function(err), true})
	env = env.MustDefine(common.Symbol("eqv?"), []int{0}, &common.Variable{common.Function(eqv), true})
	env = env.MustDefine(common.Symbol("syntax->datum"), []int{0}, &common.Variable{common.Function(syntaxDatum), true})
	Bindings = env.Bindings()
}

var (
	PatternQuote                      = util.Pattern(read.MustReadString("(quote datum)")[0])
	PatternSyntax                     = util.Pattern(read.MustReadString("(syntax datum)")[0])
	PatternIf                         = util.Pattern(read.MustReadString("(if condition then else)")[0])
	PatternLetStar                    = util.Pattern(read.MustReadString("(let* ((name init) ...) body ...)")[0])
	PatternBegin                      = util.Pattern(read.MustReadString("(begin body ...)")[0])
	PatternLambda                     = util.Pattern(read.MustReadString("(lambda (formals ...) body ...)")[0])
	PatternDefineLambda               = util.Pattern(read.MustReadString("(define (name formals ...) body ...)")[0])
	PatternDefine                     = util.Pattern(read.MustReadString("(define name value)")[0])
	PatternDefineSyntax               = util.Pattern(read.MustReadString("(define-syntax name value)")[0])
	PatternSyntaxCase                 = util.Pattern(read.MustReadString("(syntax-case input (literal ...) clause ...)")[0])
	PatternSyntaxCaseClause           = util.Pattern(read.MustReadString("(pattern output)")[0])
	PatternSyntaxCaseClauseWithFender = util.Pattern(read.MustReadString("(pattern fender output)")[0])
	PatternSet                        = util.Pattern(read.MustReadString("(set! name expression)")[0])
)

func transformQuote(c common.Continuation, args ...common.Datum) (common.EvaluationResult, error) {
	result, ok, err := util.MatchSyntax(args[0].(common.WrappedSyntax), PatternQuote, nil)
	if err != nil {
		return common.ErrorC(err)
	} else if !ok {
		return common.ErrorC(fmt.Errorf("quote: bad syntax"))
	}
	return common.CallC(c, common.QuoteForm{result[common.Symbol("datum")].(common.WrappedSyntax).Datum()})
}

func transformSyntax(c common.Continuation, args ...common.Datum) (common.EvaluationResult, error) {
	result, ok, err := util.MatchSyntax(args[0].(common.WrappedSyntax), PatternQuote, nil)
	if err != nil {
		return common.ErrorC(err)
	} else if !ok {
		return common.ErrorC(fmt.Errorf("syntax: bad syntax"))
	}
	return common.CallC(c, common.SyntaxForm{result[common.Symbol("datum")].(common.WrappedSyntax)})
}

func transformIf(c common.Continuation, args ...common.Datum) (common.EvaluationResult, error) {
	result, ok, err := util.MatchSyntax(args[0].(common.WrappedSyntax), PatternIf, nil)
	if err != nil {
		return common.ErrorC(err)
	} else if !ok {
		return common.ErrorC(fmt.Errorf("if: bad syntax"))
	}
	form := common.IfForm{
		result[common.Symbol("condition")],
		result[common.Symbol("then")],
		result[common.Symbol("else")],
	}
	return common.CallC(c, form)
}

func transformLetStar(c common.Continuation, args ...common.Datum) (common.EvaluationResult, error) {
	result, ok, err := util.MatchSyntax(args[0].(common.WrappedSyntax), PatternLetStar, nil)
	if err != nil {
		return common.ErrorC(err)
	} else if !ok {
		return common.ErrorC(fmt.Errorf("let*: bad syntax"))
	}
	var names []common.Symbol
	for _, name := range result[common.Symbol("name")].([]interface{}) {
		name, _, ok := name.(common.WrappedSyntax).Identifier()
		if !ok {
			return common.ErrorC(fmt.Errorf("let*: bad syntax"))
		}
		names = append(names, name)
	}
	var inits []common.Form
	for _, init := range result[common.Symbol("init")].([]interface{}) {
		inits = append(inits, init)
	}
	var forms []common.Form
	for _, form := range result[common.Symbol("body")].([]interface{}) {
		forms = append(forms, form)
	}
	var form common.Datum = common.BeginForm{forms}
	for i := len(names) - 1; i >= 0; i-- {
		form = common.LetForm{names[i], inits[i], form}
	}
	return common.CallC(c, form)
}

func transformBegin(c common.Continuation, args ...common.Datum) (common.EvaluationResult, error) {
	result, ok, err := util.MatchSyntax(args[0].(common.WrappedSyntax), PatternBegin, nil)
	if err != nil {
		return common.ErrorC(err)
	} else if !ok {
		return common.ErrorC(fmt.Errorf("begin: bad syntax"))
	}
	var forms []common.Form
	for _, form := range result[common.Symbol("body")].([]interface{}) {
		forms = append(forms, form)
	}
	return common.CallC(c, common.BeginForm{forms})
}

func transformLambda(c common.Continuation, args ...common.Datum) (common.EvaluationResult, error) {
	result, ok, err := util.MatchSyntax(args[0].(common.WrappedSyntax), PatternLambda, nil)
	if err != nil {
		return common.ErrorC(err)
	} else if !ok {
		return common.ErrorC(fmt.Errorf("lambda: bad syntax"))
	}
	form, err := makeLambdaFromResult(result)
	if err != nil {
		return common.ErrorC(err)
	}
	return common.CallC(c, form)
}

func transformDefine(c common.Continuation, args ...common.Datum) (common.EvaluationResult, error) {
	var (
		name common.Symbol
		form common.Datum
	)
	// TODO implement other define forms
	// TODO implement define procedure with syntax transformation
	if result, ok, err := util.MatchSyntax(args[0].(common.WrappedSyntax), PatternDefineLambda, nil); err != nil {
		return common.ErrorC(err)
	} else if ok {
		name, _, ok = result[common.Symbol("name")].(common.WrappedSyntax).Identifier()
		if !ok {
			return common.ErrorC(fmt.Errorf("define: bad syntax"))
		}
		lambda, err := makeLambdaFromResult(result)
		if err != nil {
			return common.ErrorC(err)
		}
		form = lambda
	} else if result, ok, err := util.MatchSyntax(args[0].(common.WrappedSyntax), PatternDefine, nil); err != nil {
		return common.ErrorC(err)
	} else if ok {
		name, _, ok = result[common.Symbol("name")].(common.WrappedSyntax).Identifier()
		if !ok {
			return common.ErrorC(fmt.Errorf("define: bad syntax"))
		}
		form = result[common.Symbol("value")]
	} else {
		return common.ErrorC(fmt.Errorf("define: bad syntax"))
	}
	return common.CallC(c, common.DefineForm{name, form})
}

func makeLambdaFromResult(result map[common.Symbol]interface{}) (common.LambdaForm, error) {
	var formals []common.Symbol
	for _, s := range result[common.Symbol("formals")].([]interface{}) {
		if formal, _, ok := s.(common.WrappedSyntax).Identifier(); ok {
			formals = append(formals, formal)
		} else {
			return common.LambdaForm{}, fmt.Errorf("lambda: bad syntax")
		}
	}
	var forms []common.Form
	for _, form := range result[common.Symbol("body")].([]interface{}) {
		forms = append(forms, form)
	}
	var body common.Datum = common.BeginForm{forms}
	return common.LambdaForm{formals, body}, nil
}

func transformDefineSyntax(c common.Continuation, args ...common.Datum) (common.EvaluationResult, error) {
	result, ok, err := util.MatchSyntax(args[0].(common.WrappedSyntax), PatternDefineSyntax, nil)
	if err != nil {
		return common.ErrorC(err)
	} else if !ok {
		return common.ErrorC(fmt.Errorf("define-syntax: bad syntax"))
	}
	name, _, ok := result[common.Symbol("name")].(common.WrappedSyntax).Identifier()
	if !ok {
		return common.ErrorC(fmt.Errorf("define-syntax: bad syntax"))
	}
	form := result[common.Symbol("value")]
	return common.CallC(c, common.DefineSyntaxForm{name, form})
}

func transformSyntaxCase(c common.Continuation, args ...common.Datum) (common.EvaluationResult, error) {
	result, ok, err := util.MatchSyntax(args[0].(common.WrappedSyntax), PatternSyntaxCase, nil)
	if err != nil {
		return common.ErrorC(err)
	} else if !ok {
		return common.ErrorC(fmt.Errorf("syntax-case: bad syntax"))
	}
	input := result[common.Symbol("input")]
	var literals []common.Symbol
	for _, literal := range result[common.Symbol("literal")].([]interface{}) {
		name, _, ok := literal.(common.WrappedSyntax).Identifier()
		if !ok {
			return common.ErrorC(fmt.Errorf("syntax-case: bad syntax"))
		}
		literals = append(literals, name)
	}
	var (
		patterns []common.Form
		fenders  []common.Form
		outputs  []common.Form
	)
	for _, clause := range result[common.Symbol("clause")].([]interface{}) {
		var (
			pattern common.Form
			fender  common.Form
			output  common.Form
		)
		if result, ok, err := util.MatchSyntax(clause.(common.WrappedSyntax), PatternSyntaxCaseClause, nil); err != nil {
			return common.ErrorC(err)
		} else if ok {
			pattern = result[common.Symbol("pattern")]
			fender = common.NewWrappedSyntax(common.Boolean(true))
			output = result[common.Symbol("output")]
		} else if result, ok, err := util.MatchSyntax(clause.(common.WrappedSyntax), PatternSyntaxCaseClauseWithFender, nil); err != nil {
			return common.ErrorC(err)
		} else if ok {
			pattern = result[common.Symbol("pattern")]
			fender = result[common.Symbol("fender")]
			output = result[common.Symbol("output")]
		} else {
			return common.ErrorC(fmt.Errorf("syntax-case: bad syntax"))
		}
		patterns = append(patterns, pattern)
		fenders = append(fenders, fender)
		outputs = append(outputs, output)
	}
	return common.CallC(c, common.SyntaxCaseForm{input, literals, patterns, fenders, outputs})
}

func transformSet(c common.Continuation, args ...common.Datum) (common.EvaluationResult, error) {
	result, ok, err := util.MatchSyntax(args[0].(common.WrappedSyntax), PatternSet, nil)
	if err != nil {
		return common.ErrorC(err)
	} else if !ok {
		return common.ErrorC(fmt.Errorf("set!: bad syntax"))
	}
	name, _, ok := result[common.Symbol("name")].(common.WrappedSyntax).Identifier()
	if !ok {
		return common.ErrorC(fmt.Errorf("set!: bad syntax"))
	}
	form := result[common.Symbol("expression")]
	return common.CallC(c, common.SetForm{name, form})
}

func cons(c common.Continuation, args ...common.Datum) (common.EvaluationResult, error) {
	if len(args) != 2 {
		return common.ErrorC(fmt.Errorf("cons: wrong arity"))
	}
	return common.CallC(c, common.Pair{args[0], args[1]})
}

func car(c common.Continuation, args ...common.Datum) (common.EvaluationResult, error) {
	if len(args) != 1 {
		return common.ErrorC(fmt.Errorf("car: wrong arity"))
	}
	pair, ok := args[0].(common.Pair)
	if !ok {
		return common.ErrorC(fmt.Errorf("car: expected pair"))
	}
	return common.CallC(c, pair.First)
}

func cdr(c common.Continuation, args ...common.Datum) (common.EvaluationResult, error) {
	if len(args) != 1 {
		return common.ErrorC(fmt.Errorf("cdr: wrong arity"))
	}
	pair, ok := args[0].(common.Pair)
	if !ok {
		return common.ErrorC(fmt.Errorf("cdr: expected pair"))
	}
	return common.CallC(c, pair.Rest)
}

func null(c common.Continuation, args ...common.Datum) (common.EvaluationResult, error) {
	if len(args) != 1 {
		return common.ErrorC(fmt.Errorf("null?: wrong arity"))
	}
	return common.CallC(c, common.Boolean(args[0] == nil))
}

func write(c common.Continuation, args ...common.Datum) (common.EvaluationResult, error) {
	if len(args) != 1 {
		return common.ErrorC(fmt.Errorf("write: wrong arity"))
	}
	fmt.Print(common.Write(args[0]))
	return common.CallC(c, common.Void)
}

func callWithCurrentContinuation(c common.Continuation, args ...common.Datum) (common.EvaluationResult, error) {
	if len(args) != 1 {
		return common.ErrorC(fmt.Errorf("call/cc: wrong arity"))
	}
	return eval.Apply(c, args[0], common.ContinuationProcedure{c})
}

type stringError string

func (s stringError) Error() string {
	return string(s)
}

func err(c common.Continuation, args ...common.Datum) (common.EvaluationResult, error) {
	if len(args) != 1 {
		return common.ErrorC(fmt.Errorf("error: wrong arity"))
	}
	message, ok := args[0].(common.String)
	if !ok {
		return common.ErrorC(fmt.Errorf("error: expected string"))
	}
	return common.ErrorC(stringError(message))
}

func eqv(c common.Continuation, args ...common.Datum) (common.EvaluationResult, error) {
	if len(args) != 2 {
		return common.ErrorC(fmt.Errorf("eqv?: wrong arity"))
	}
	return common.CallC(c, common.Boolean(args[0] == args[1]))
}

func syntaxDatum(c common.Continuation, args ...common.Datum) (common.EvaluationResult, error) {
	if len(args) != 1 {
		return common.ErrorC(fmt.Errorf("syntax->datum: wrong arity"))
	}
	syntax, ok := args[0].(common.WrappedSyntax)
	if !ok {
		return common.ErrorC(fmt.Errorf("syntax->datum: expected syntax"))
	}
	return common.CallC(c, syntax.Datum())
}
