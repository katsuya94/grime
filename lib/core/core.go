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

var Bindings = map[common.Symbol]common.Binding{
	common.Symbol("quote"):         common.Keyword{common.Function(transformQuote)},
	common.Symbol("syntax"):        common.Keyword{common.Function(transformSyntax)},
	common.Symbol("if"):            common.Keyword{common.Function(transformIf)},
	common.Symbol("let*"):          common.Keyword{common.Function(transformLetStar)},
	common.Symbol("begin"):         common.Keyword{common.Function(transformBegin)},
	common.Symbol("lambda"):        common.Keyword{common.Function(transformLambda)},
	common.Symbol("define"):        common.Keyword{common.Function(transformDefine)},
	common.Symbol("define-syntax"): common.Keyword{common.Function(transformDefineSyntax)},
	common.Symbol("set!"):          common.Keyword{common.Function(transformSet)},
	common.Symbol("cons"):          &common.Variable{common.Function(cons)},
	common.Symbol("car"):           &common.Variable{common.Function(car)},
	common.Symbol("cdr"):           &common.Variable{common.Function(cdr)},
	common.Symbol("null?"):         &common.Variable{common.Function(null)},
	common.Symbol("write"):         &common.Variable{common.Function(write)},
	common.Symbol("call/cc"):       &common.Variable{common.Function(callWithCurrentContinuation)},
	common.Symbol("error"):         &common.Variable{common.Function(err)},
	common.Symbol("eqv?"):          &common.Variable{common.Function(eqv)},
}

var (
	PatternQuote        = read.MustReadString("(quote datum)")[0]
	PatternSyntax       = read.MustReadString("(syntax datum)")[0]
	PatternIf           = read.MustReadString("(if condition then else)")[0]
	PatternLetStar      = read.MustReadString("(let* ((name init) ...) body ...)")[0]
	PatternBegin        = read.MustReadString("(begin body ...)")[0]
	PatternLambda       = read.MustReadString("(lambda (formals ...) body ...)")[0]
	PatternDefineLambda = read.MustReadString("(define (name formals ...) body ...)")[0]
	PatternDefine       = read.MustReadString("(define name value)")[0]
	PatternDefineSyntax = read.MustReadString("(define-syntax name value)")[0]
	PatternSet          = read.MustReadString("(set! name expression)")[0]
)

func transformQuote(c common.Continuation, syntax ...common.Datum) (common.EvaluationResult, error) {
	result, ok, err := util.Match(syntax[0], PatternQuote, nil)
	if err != nil {
		return common.ErrorC(err)
	} else if !ok {
		return common.ErrorC(fmt.Errorf("quote: bad syntax"))
	}
	return common.CallC(c, common.QuoteForm{result[common.Symbol("datum")]})
}

func transformSyntax(c common.Continuation, syntax ...common.Datum) (common.EvaluationResult, error) {
	result, ok, err := util.Match(syntax[0], PatternQuote, nil)
	if err != nil {
		return common.ErrorC(err)
	} else if !ok {
		return common.ErrorC(fmt.Errorf("syntax: bad syntax"))
	}
	return common.CallC(c, common.SyntaxForm{result[common.Symbol("datum")]})
}

func transformIf(c common.Continuation, syntax ...common.Datum) (common.EvaluationResult, error) {
	result, ok, err := util.Match(syntax[0], PatternIf, nil)
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

func transformLetStar(c common.Continuation, syntax ...common.Datum) (common.EvaluationResult, error) {
	result, ok, err := util.Match(syntax[0], PatternLetStar, nil)
	if err != nil {
		return common.ErrorC(err)
	} else if !ok {
		return common.ErrorC(fmt.Errorf("let*: bad syntax"))
	}
	var names []common.Symbol
	for _, name := range result[common.Symbol("name")].([]interface{}) {
		if name, ok := name.(common.Symbol); ok {
			names = append(names, name)
		} else {
			return common.ErrorC(fmt.Errorf("let*: bad syntax"))
		}
	}
	var inits []common.Datum
	for _, init := range result[common.Symbol("init")].([]interface{}) {
		inits = append(inits, init)
	}
	var forms []common.Datum
	for _, form := range result[common.Symbol("body")].([]interface{}) {
		forms = append(forms, form)
	}
	var form common.Datum = common.BeginForm{forms}
	for i := len(names) - 1; i >= 0; i-- {
		form = common.LetForm{names[i], inits[i], form}
	}
	return common.CallC(c, form)
}

func transformBegin(c common.Continuation, syntax ...common.Datum) (common.EvaluationResult, error) {
	result, ok, err := util.Match(syntax[0], PatternBegin, nil)
	if err != nil {
		return common.ErrorC(err)
	} else if !ok {
		return common.ErrorC(fmt.Errorf("begin: bad syntax"))
	}
	var forms []common.Datum
	for _, form := range result[common.Symbol("body")].([]interface{}) {
		forms = append(forms, form)
	}
	return common.CallC(c, common.BeginForm{forms})
}

func transformLambda(c common.Continuation, syntax ...common.Datum) (common.EvaluationResult, error) {
	result, ok, err := util.Match(syntax[0], PatternLambda, nil)
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

func transformDefine(c common.Continuation, syntax ...common.Datum) (common.EvaluationResult, error) {
	var (
		name common.Symbol
		form common.Datum
	)
	// TODO implement other define forms
	// TODO implement define procedure with syntax transformation
	if result, ok, err := util.Match(syntax[0], PatternDefineLambda, nil); err != nil {
		return common.ErrorC(err)
	} else if ok {
		name, ok = result[common.Symbol("name")].(common.Symbol)
		if !ok {
			return common.ErrorC(fmt.Errorf("define: bad syntax"))
		}
		lambda, err := makeLambdaFromResult(result)
		if err != nil {
			return common.ErrorC(err)
		}
		form = lambda
	} else if result, ok, err := util.Match(syntax[0], PatternDefine, nil); err != nil {
		return common.ErrorC(err)
	} else if ok {
		name, ok = result[common.Symbol("name")].(common.Symbol)
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
		if formal, ok := s.(common.Symbol); ok {
			formals = append(formals, formal)
		} else {
			return common.LambdaForm{}, fmt.Errorf("lambda: bad syntax")
		}
	}
	var forms []common.Datum
	for _, form := range result[common.Symbol("body")].([]interface{}) {
		forms = append(forms, form)
	}
	var body common.Datum = common.BeginForm{forms}
	return common.LambdaForm{formals, body}, nil
}

func transformDefineSyntax(c common.Continuation, syntax ...common.Datum) (common.EvaluationResult, error) {
	result, ok, err := util.Match(syntax[0], PatternDefineSyntax, nil)
	if err != nil {
		return common.ErrorC(err)
	} else if !ok {
		return common.ErrorC(fmt.Errorf("define-syntax: bad syntax"))
	}
	name, ok := result[common.Symbol("name")].(common.Symbol)
	if !ok {
		return common.ErrorC(fmt.Errorf("define-syntax: bad syntax"))
	}
	form := result[common.Symbol("value")]
	return common.CallC(c, common.DefineSyntaxForm{name, form})
}

func transformSet(c common.Continuation, syntax ...common.Datum) (common.EvaluationResult, error) {
	result, ok, err := util.Match(syntax[0], PatternSet, nil)
	if err != nil {
		return common.ErrorC(err)
	} else if !ok {
		return common.ErrorC(fmt.Errorf("set!: bad syntax"))
	}
	name, ok := result[common.Symbol("name")].(common.Symbol)
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
