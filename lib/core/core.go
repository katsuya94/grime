package core

import (
	"fmt"

	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/read"
	"github.com/katsuya94/grime/runtime"
)

var Library *runtime.Library = runtime.MustNewEmptyLibrary([]common.Symbol{common.Symbol("core")}, []int{})

var Bindings common.BindingSet

var (
	setKeyword        = &common.Keyword{common.Function(transformSet)}
	underscoreKeyword = &common.Keyword{common.Function(func(common.Continuation, ...common.Datum) (common.EvaluationResult, error) {
		return nil, fmt.Errorf("cannot expand underscore")
	})}
	ellipsisKeyword = &common.Keyword{common.Function(func(common.Continuation, ...common.Datum) (common.EvaluationResult, error) {
		return nil, fmt.Errorf("cannot expand ellipsis")
	})}
)

func init() {
	env := common.EmptyEnvironment
	env = env.MustDefine(common.Symbol("quote"), []int{0}, &common.Keyword{common.Function(transformQuote)})
	env = env.MustDefine(common.Symbol("syntax"), []int{0}, &common.Keyword{common.Function(transformSyntax)})
	env = env.MustDefine(common.Symbol("if"), []int{0}, &common.Keyword{common.Function(transformIf)})
	env = env.MustDefine(common.Symbol("~let"), []int{0}, &common.Keyword{common.Function(transformLet)})
	env = env.MustDefine(common.Symbol("begin"), []int{0}, &common.Keyword{common.Function(transformBegin)})
	env = env.MustDefine(common.Symbol("lambda"), []int{0}, &common.Keyword{common.Function(transformLambda)})
	env = env.MustDefine(common.Symbol("~define"), []int{0}, &common.Keyword{common.Function(transformDefine)})
	env = env.MustDefine(common.Symbol("define-syntax"), []int{0}, &common.Keyword{common.Function(transformDefineSyntax)})
	env = env.MustDefine(common.Symbol("syntax-case"), []int{0}, &common.Keyword{common.Function(transformSyntaxCase)})
	env = env.MustDefine(common.Symbol("set!"), []int{0}, setKeyword)
	env = env.MustDefine(common.Symbol("_"), []int{0}, underscoreKeyword)
	env = env.MustDefine(common.Symbol("..."), []int{0}, ellipsisKeyword)
	env = env.MustDefine(common.Symbol("not"), []int{0}, &common.Variable{common.Function(not)})
	env = env.MustDefine(common.Symbol("cons"), []int{0}, &common.Variable{common.Function(cons)})
	env = env.MustDefine(common.Symbol("car"), []int{0}, &common.Variable{common.Function(car)})
	env = env.MustDefine(common.Symbol("cdr"), []int{0}, &common.Variable{common.Function(cdr)})
	env = env.MustDefine(common.Symbol("null?"), []int{0}, &common.Variable{common.Function(null)})
	env = env.MustDefine(common.Symbol("pair?"), []int{0}, &common.Variable{common.Function(pair)})
	env = env.MustDefine(common.Symbol("proc?"), []int{0}, &common.Variable{common.Function(proc)})
	env = env.MustDefine(common.Symbol("write"), []int{0}, &common.Variable{common.Function(write)})
	env = env.MustDefine(common.Symbol("call/cc"), []int{0}, &common.Variable{common.Function(callWithCurrentContinuation)})
	env = env.MustDefine(common.Symbol("error"), []int{0}, &common.Variable{common.Function(err)})
	env = env.MustDefine(common.Symbol("eqv?"), []int{0}, &common.Variable{common.Function(eqv)})
	env = env.MustDefine(common.Symbol("syntax->datum"), []int{0}, &common.Variable{common.Function(syntaxDatum)})
	env = env.MustDefine(common.Symbol("identifier?"), []int{0}, &common.Variable{common.Function(identifier)})
	env = env.MustDefine(common.Symbol("generate-temporaries"), []int{0}, &common.Variable{common.Function(generateTemporaries)})
	env = env.MustDefine(common.Symbol("list"), []int{0}, &common.Variable{common.Function(list)})
	Bindings = env.Bindings()
}

var (
	PatternQuote                      = common.Pattern(read.MustReadDatum("(quote datum)"))
	PatternSyntax                     = common.Pattern(read.MustReadDatum("(syntax datum)"))
	PatternIf                         = common.Pattern(read.MustReadDatum("(if condition then else)"))
	PatternLet                        = common.Pattern(read.MustReadDatum("(let (name init) body ...)"))
	PatternBegin                      = common.Pattern(read.MustReadDatum("(begin body ...)"))
	PatternLambda                     = common.Pattern(read.MustReadDatum("(lambda (formals ...) body ...)"))
	PatternDefine                     = common.Pattern(read.MustReadDatum("(~define name value)"))
	PatternDefineSyntax               = common.Pattern(read.MustReadDatum("(define-syntax name value)"))
	PatternSyntaxCase                 = common.Pattern(read.MustReadDatum("(syntax-case input (literal ...) clause ...)"))
	PatternSyntaxCaseClause           = common.Pattern(read.MustReadDatum("(pattern output)"))
	PatternSyntaxCaseClauseWithFender = common.Pattern(read.MustReadDatum("(pattern fender output)"))
	PatternSet                        = common.Pattern(read.MustReadDatum("(set! name expression)"))
)

func transformQuote(c common.Continuation, args ...common.Datum) (common.EvaluationResult, error) {
	result, ok, err := common.MatchSyntax(args[0], PatternQuote, nil)
	if err != nil {
		return common.ErrorC(err)
	} else if !ok {
		return common.ErrorC(fmt.Errorf("quote: bad syntax"))
	}
	return common.CallC(c, QuoteForm{result[common.Symbol("datum")].(common.WrappedSyntax).Datum()})
}

func transformSyntax(c common.Continuation, args ...common.Datum) (common.EvaluationResult, error) {
	result, ok, err := common.MatchSyntax(args[0], PatternQuote, nil)
	if err != nil {
		return common.ErrorC(err)
	} else if !ok {
		return common.ErrorC(fmt.Errorf("syntax: bad syntax"))
	}
	return common.CallC(c, SyntaxForm{result[common.Symbol("datum")].(common.WrappedSyntax)})
}

func transformIf(c common.Continuation, args ...common.Datum) (common.EvaluationResult, error) {
	result, ok, err := common.MatchSyntax(args[0], PatternIf, nil)
	if err != nil {
		return common.ErrorC(err)
	} else if !ok {
		return common.ErrorC(fmt.Errorf("if: bad syntax"))
	}
	form := IfForm{
		result[common.Symbol("condition")],
		result[common.Symbol("then")],
		result[common.Symbol("else")],
	}
	return common.CallC(c, form)
}

func transformLet(c common.Continuation, args ...common.Datum) (common.EvaluationResult, error) {
	result, ok, err := common.MatchSyntax(args[0], PatternLet, nil)
	if err != nil {
		return common.ErrorC(err)
	} else if !ok {
		return common.ErrorC(fmt.Errorf("~let: bad syntax"))
	}
	syntax, ok := result[common.Symbol("name")].(common.WrappedSyntax)
	if !ok {
		return common.ErrorC(fmt.Errorf("~let: bad syntax"))
	}
	if !syntax.IsIdentifier() {
		return common.ErrorC(fmt.Errorf("~let: bad syntax"))
	}
	init := result[common.Symbol("init")]
	var forms []common.Datum
	for _, form := range result[common.Symbol("body")].([]interface{}) {
		forms = append(forms, form)
	}
	return common.CallC(c, LetForm{syntax, init, forms})
}

func transformBegin(c common.Continuation, args ...common.Datum) (common.EvaluationResult, error) {
	result, ok, err := common.MatchSyntax(args[0], PatternBegin, nil)
	if err != nil {
		return common.ErrorC(err)
	} else if !ok {
		return common.ErrorC(fmt.Errorf("begin: bad syntax"))
	}
	var forms []common.Datum
	for _, form := range result[common.Symbol("body")].([]interface{}) {
		forms = append(forms, form)
	}
	return common.CallC(c, BeginForm{forms})
}

func transformLambda(c common.Continuation, args ...common.Datum) (common.EvaluationResult, error) {
	result, ok, err := common.MatchSyntax(args[0], PatternLambda, nil)
	if err != nil {
		return common.ErrorC(err)
	} else if !ok {
		return common.ErrorC(fmt.Errorf("lambda: bad syntax"))
	}
	var formals []common.WrappedSyntax
	for _, syntax := range result[common.Symbol("formals")].([]interface{}) {
		identifier, ok := syntax.(common.WrappedSyntax)
		if !(ok && identifier.IsIdentifier()) {
			return LambdaForm{}, fmt.Errorf("lambda: bad syntax")
		}
		formals = append(formals, identifier)
	}
	var forms []common.Datum
	for _, form := range result[common.Symbol("body")].([]interface{}) {
		forms = append(forms, form)
	}
	return common.CallC(c, LambdaForm{formals, forms})
}

func transformDefine(c common.Continuation, args ...common.Datum) (common.EvaluationResult, error) {
	result, ok, err := common.MatchSyntax(args[0], PatternDefine, nil)
	if err != nil {
		return common.ErrorC(err)
	} else if !ok {
		return common.ErrorC(fmt.Errorf("~define: bad syntax"))
	}
	identifier, ok := result[common.Symbol("name")].(common.WrappedSyntax)
	if !(ok && identifier.IsIdentifier()) {
		return common.ErrorC(fmt.Errorf("~define: bad syntax"))
	}
	form := result[common.Symbol("value")]
	return common.CallC(c, DefineForm{identifier, form})
}

func transformDefineSyntax(c common.Continuation, args ...common.Datum) (common.EvaluationResult, error) {
	result, ok, err := common.MatchSyntax(args[0], PatternDefineSyntax, nil)
	if err != nil {
		return common.ErrorC(err)
	} else if !ok {
		return common.ErrorC(fmt.Errorf("define-syntax: bad syntax"))
	}
	identifier, ok := result[common.Symbol("name")].(common.WrappedSyntax)
	if !(ok && identifier.IsIdentifier()) {
		return common.ErrorC(fmt.Errorf("define-syntax: bad syntax"))
	}
	form := result[common.Symbol("value")]
	return common.CallC(c, DefineSyntaxForm{identifier, form})
}

func transformSyntaxCase(c common.Continuation, args ...common.Datum) (common.EvaluationResult, error) {
	result, ok, err := common.MatchSyntax(args[0], PatternSyntaxCase, nil)
	if err != nil {
		return common.ErrorC(err)
	} else if !ok {
		return common.ErrorC(fmt.Errorf("syntax-case: bad syntax"))
	}
	input := result[common.Symbol("input")]
	var literals []common.WrappedSyntax
	for _, literal := range result[common.Symbol("literal")].([]interface{}) {
		identifier, ok := literal.(common.WrappedSyntax)
		if !(ok && identifier.IsIdentifier()) {
			return common.ErrorC(fmt.Errorf("syntax-case: bad syntax"))
		}
		literals = append(literals, identifier)
	}
	var (
		patterns []common.Datum
		fenders  []common.Datum
		outputs  []common.Datum
	)
	for _, clause := range result[common.Symbol("clause")].([]interface{}) {
		var (
			pattern common.Datum
			fender  common.Datum
			output  common.Datum
		)
		if result, ok, err := common.MatchSyntax(clause, PatternSyntaxCaseClause, nil); err != nil {
			return common.ErrorC(err)
		} else if ok {
			pattern = result[common.Symbol("pattern")]
			fender = common.NewWrappedSyntax(common.Boolean(true))
			output = result[common.Symbol("output")]
		} else if result, ok, err := common.MatchSyntax(clause, PatternSyntaxCaseClauseWithFender, nil); err != nil {
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
	return common.CallC(c, SyntaxCaseForm{input, literals, patterns, fenders, outputs})
}

func transformSet(c common.Continuation, args ...common.Datum) (common.EvaluationResult, error) {
	result, ok, err := common.MatchSyntax(args[0], PatternSet, nil)
	if err != nil {
		return common.ErrorC(err)
	} else if !ok {
		return common.ErrorC(fmt.Errorf("set!: bad syntax"))
	}
	identifier, ok := result[common.Symbol("name")].(common.WrappedSyntax)
	if !(ok && identifier.IsIdentifier()) {
		return common.ErrorC(fmt.Errorf("set!: bad syntax"))
	}
	form := result[common.Symbol("expression")]
	return common.CallC(c, SetForm{identifier, form})
}

func not(c common.Continuation, args ...common.Datum) (common.EvaluationResult, error) {
	if len(args) != 1 {
		return common.ErrorC(fmt.Errorf("not: wrong arity"))
	}
	boolean, ok := args[0].(common.Boolean)
	if !ok {
		return common.CallC(c, common.Boolean(false))
	}
	return common.CallC(c, common.Boolean(!boolean))
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
	return common.CallC(c, common.Boolean(args[0] == common.Null))
}

func pair(c common.Continuation, args ...common.Datum) (common.EvaluationResult, error) {
	if len(args) != 1 {
		return common.ErrorC(fmt.Errorf("pair?: wrong arity"))
	}
	_, ok := args[0].(common.Pair)
	return common.CallC(c, common.Boolean(ok))
}

func proc(c common.Continuation, args ...common.Datum) (common.EvaluationResult, error) {
	if len(args) != 1 {
		return common.ErrorC(fmt.Errorf("proc?: wrong arity"))
	}
	_, ok := args[0].(common.Procedure)
	return common.CallC(c, common.Boolean(ok))
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
	return common.Apply(c, args[0], common.ContinuationProcedure{c})
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
	datum, err := syntaxDatumRecursive(args[0])
	if err != nil {
		return common.ErrorC(err)
	}
	return common.CallC(c, datum)
}

func syntaxDatumRecursive(syntax common.Datum) (common.Datum, error) {
	switch syntax := syntax.(type) {
	case common.WrappedSyntax:
		return syntax.Datum(), nil
	case common.Pair:
		first, err := syntaxDatumRecursive(syntax.First)
		if err != nil {
			return nil, err
		}
		rest, err := syntaxDatumRecursive(syntax.Rest)
		if err != nil {
			return nil, err
		}
		return common.Pair{first, rest}, nil
	default:
		return nil, fmt.Errorf("syntax->datum: expected syntax")
	}
}

func identifier(c common.Continuation, args ...common.Datum) (common.EvaluationResult, error) {
	if len(args) != 1 {
		return common.ErrorC(fmt.Errorf("identifier?: wrong arity"))
	}
	syntax, ok := args[0].(common.WrappedSyntax)
	return common.CallC(c, common.Boolean(ok && syntax.IsIdentifier()))
}

var temporaryIdentifiers int

func generateTemporary() common.WrappedSyntax {
	symbol := common.Symbol(fmt.Sprintf(".%v", temporaryIdentifiers))
	temporaryIdentifiers++
	return common.NewWrappedSyntax(symbol)
}

func generateTemporaries(c common.Continuation, args ...common.Datum) (common.EvaluationResult, error) {
	if len(args) != 1 {
		return common.ErrorC(fmt.Errorf("generate-temporaries: wrong arity"))
	}
	list := args[0]
	n := 0
	for {
		syntax, ok := list.(common.WrappedSyntax)
		if ok {
			list = syntax.Datum()
		}
		switch datum := list.(type) {
		case common.Pair:
			n++
			list = datum.Rest
			continue
		default:
			if datum != common.Null {
				return common.ErrorC(fmt.Errorf("generate-temporaries: expected proper list"))
			}
		}
		break
	}
	var temporaries common.Datum = common.Null
	for i := 0; i < n; i++ {
		identifier := generateTemporary()
		temporaries = common.Pair{identifier, temporaries}
	}
	return common.CallC(c, temporaries)
}

func list(c common.Continuation, args ...common.Datum) (common.EvaluationResult, error) {
	var list common.Datum = common.Null
	for i := len(args) - 1; i >= 0; i-- {
		list = common.Pair{args[i], list}
	}
	return common.CallC(c, list)
}
