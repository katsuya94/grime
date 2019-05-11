package core

import (
	"fmt"

	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/read"
	"github.com/katsuya94/grime/runtime"
)

var Library *runtime.Library = runtime.MustNewEmptyLibrary([]common.Symbol{common.Symbol("core")}, []int{})

var Bindings = common.BindingSet{
	0: map[common.Symbol]common.Binding{
		common.Symbol("quote"):                &common.Keyword{common.Function(transformQuote), -1},
		common.Symbol("syntax"):               &common.Keyword{common.Function(transformSyntax), -1},
		common.Symbol("if"):                   &common.Keyword{common.Function(transformIf), -1},
		common.Symbol("~let"):                 &common.Keyword{common.Function(transformLet), -1},
		common.Symbol("begin"):                &common.Keyword{common.Function(transformBegin), -1},
		common.Symbol("lambda"):               &common.Keyword{common.Function(transformLambda), -1},
		common.Symbol("~define"):              &common.Keyword{common.Function(transformDefine), -1},
		common.Symbol("define-syntax"):        &common.Keyword{common.Function(transformDefineSyntax), -1},
		common.Symbol("syntax-case"):          &common.Keyword{common.Function(transformSyntaxCase), -1},
		common.Symbol("set!"):                 setKeyword,
		common.Symbol("_"):                    common.UnderscoreKeyword,
		common.Symbol("..."):                  common.EllipsisKeyword,
		common.Symbol("not"):                  &common.Variable{common.Function(not), -1},
		common.Symbol("cons"):                 &common.Variable{common.Function(cons), -1},
		common.Symbol("car"):                  &common.Variable{common.Function(car), -1},
		common.Symbol("cdr"):                  &common.Variable{common.Function(cdr), -1},
		common.Symbol("null?"):                &common.Variable{common.Function(null), -1},
		common.Symbol("pair?"):                &common.Variable{common.Function(pair), -1},
		common.Symbol("proc?"):                &common.Variable{common.Function(proc), -1},
		common.Symbol("write"):                &common.Variable{common.Function(write), -1},
		common.Symbol("call/cc"):              &common.Variable{common.Function(callWithCurrentContinuation), -1},
		common.Symbol("error"):                &common.Variable{common.Function(err), -1},
		common.Symbol("eqv?"):                 &common.Variable{common.Function(eqv), -1},
		common.Symbol("syntax->datum"):        &common.Variable{common.Function(syntaxDatum), -1},
		common.Symbol("datum->syntax"):        &common.Variable{common.Function(datumSyntax), -1},
		common.Symbol("identifier?"):          &common.Variable{common.Function(identifier), -1},
		common.Symbol("generate-temporaries"): &common.Variable{common.Function(generateTemporaries), -1},
		common.Symbol("list"):                 &common.Variable{common.Function(list), -1},
		common.Symbol("bound-identifier=?"):   &common.Variable{common.Function(boundIdentifier), -1},
		common.Symbol("free-identifier=?"):    &common.Variable{common.Function(freeIdentifier), -1},
	},
}

var setKeyword = &common.Keyword{common.Function(transformSet), -1}

var (
	PatternQuote                      = common.MustCompileSimplePattern(read.MustReadDatum("(quote datum)"))
	PatternSyntax                     = common.MustCompileSimplePattern(read.MustReadDatum("(syntax datum)"))
	PatternIf                         = common.MustCompileSimplePattern(read.MustReadDatum("(if condition then else)"))
	PatternLet                        = common.MustCompileSimplePattern(read.MustReadDatum("(let (name init) body ...)"))
	PatternBegin                      = common.MustCompileSimplePattern(read.MustReadDatum("(begin body ...)"))
	PatternLambda                     = common.MustCompileSimplePattern(read.MustReadDatum("(lambda (formals ...) body ...)"))
	PatternDefine                     = common.MustCompileSimplePattern(read.MustReadDatum("(~define name value)"))
	PatternDefineSyntax               = common.MustCompileSimplePattern(read.MustReadDatum("(define-syntax name value)"))
	PatternSyntaxCase                 = common.MustCompileSimplePattern(read.MustReadDatum("(syntax-case input (literal ...) clause ...)"))
	PatternSyntaxCaseClause           = common.MustCompileSimplePattern(read.MustReadDatum("(pattern output)"))
	PatternSyntaxCaseClauseWithFender = common.MustCompileSimplePattern(read.MustReadDatum("(pattern fender output)"))
	PatternSet                        = common.MustCompileSimplePattern(read.MustReadDatum("(set! name expression)"))
)

func transformQuote(c common.Continuation, args ...common.Datum) (common.Evaluation, error) {
	result, ok := PatternQuote.Match(common.NewSyntax(args[0]))
	if !ok {
		return common.ErrorC(fmt.Errorf("quote: bad syntax"))
	}
	return common.CallC(c, QuoteForm{result[common.Symbol("datum")].(common.Syntax).Unwrap()})
}

func transformSyntax(c common.Continuation, args ...common.Datum) (common.Evaluation, error) {
	result, ok := PatternSyntax.Match(common.NewSyntax(args[0]))
	if !ok {
		return common.ErrorC(fmt.Errorf("syntax: bad syntax"))
	}
	return common.CallC(c, SyntaxForm{result[common.Symbol("datum")].(common.Syntax)})
}

func transformIf(c common.Continuation, args ...common.Datum) (common.Evaluation, error) {
	result, ok := PatternIf.Match(common.NewSyntax(args[0]))
	if !ok {
		return common.ErrorC(fmt.Errorf("if: bad syntax"))
	}
	form := IfForm{
		result[common.Symbol("condition")].(common.Syntax),
		result[common.Symbol("then")].(common.Syntax),
		result[common.Symbol("else")].(common.Syntax),
	}
	return common.CallC(c, form)
}

func transformLet(c common.Continuation, args ...common.Datum) (common.Evaluation, error) {
	result, ok := PatternLet.Match(common.NewSyntax(args[0]))
	if !ok {
		return common.ErrorC(fmt.Errorf("~let: bad syntax"))
	}
	id, ok := result[common.Symbol("name")].(common.Syntax).Identifier()
	if !ok {
		return common.ErrorC(fmt.Errorf("~let: bad syntax"))
	}
	init := result[common.Symbol("init")].(common.Syntax)
	var forms []common.Syntax
	for _, syntax := range result[common.Symbol("body")].([]interface{}) {
		forms = append(forms, syntax.(common.Syntax))
	}
	return common.CallC(c, LetForm{id, init, forms})
}

func transformBegin(c common.Continuation, args ...common.Datum) (common.Evaluation, error) {
	result, ok := PatternBegin.Match(common.NewSyntax(args[0]))
	if !ok {
		return common.ErrorC(fmt.Errorf("begin: bad syntax"))
	}
	var forms []common.Syntax
	for _, syntax := range result[common.Symbol("body")].([]interface{}) {
		forms = append(forms, syntax.(common.Syntax))
	}
	return common.CallC(c, BeginForm{forms})
}

func transformLambda(c common.Continuation, args ...common.Datum) (common.Evaluation, error) {
	result, ok := PatternLambda.Match(common.NewSyntax(args[0]))
	if !ok {
		return common.ErrorC(fmt.Errorf("lambda: bad syntax"))
	}
	var formals []common.Identifier
	for _, syntax := range result[common.Symbol("formals")].([]interface{}) {
		id, ok := syntax.(common.Syntax).Identifier()
		if !ok {
			return common.ErrorC(fmt.Errorf("lambda: bad syntax"))
		}
		formals = append(formals, id)
	}
	if common.DuplicateIdentifiers(formals...) {
		return common.ErrorC(fmt.Errorf("lambda: duplicate formals"))
	}
	var forms []common.Syntax
	for _, syntax := range result[common.Symbol("body")].([]interface{}) {
		forms = append(forms, syntax.(common.Syntax))
	}
	return common.CallC(c, LambdaForm{formals, forms})
}

func transformDefine(c common.Continuation, args ...common.Datum) (common.Evaluation, error) {
	result, ok := PatternDefine.Match(common.NewSyntax(args[0]))
	if !ok {
		return common.ErrorC(fmt.Errorf("~define: bad syntax"))
	}
	id, ok := result[common.Symbol("name")].(common.Syntax).Identifier()
	if !ok {
		return common.ErrorC(fmt.Errorf("~define: bad syntax"))
	}
	form := result[common.Symbol("value")].(common.Syntax)
	return common.CallC(c, DefineForm{id, form})
}

func transformDefineSyntax(c common.Continuation, args ...common.Datum) (common.Evaluation, error) {
	result, ok := PatternDefineSyntax.Match(common.NewSyntax(args[0]))
	if !ok {
		return common.ErrorC(fmt.Errorf("define-syntax: bad syntax"))
	}
	id, ok := result[common.Symbol("name")].(common.Syntax).Identifier()
	if !ok {
		return common.ErrorC(fmt.Errorf("define-syntax: bad syntax"))
	}
	form := result[common.Symbol("value")].(common.Syntax)
	return common.CallC(c, DefineSyntaxForm{id, form})
}

func transformSyntaxCase(c common.Continuation, args ...common.Datum) (common.Evaluation, error) {
	result, ok := PatternSyntaxCase.Match(common.NewSyntax(args[0]))
	if !ok {
		return common.ErrorC(fmt.Errorf("syntax-case: bad syntax"))
	}
	input := result[common.Symbol("input")].(common.Syntax)
	var literals []common.Identifier
	for _, literal := range result[common.Symbol("literal")].([]interface{}) {
		id, ok := literal.(common.Syntax).Identifier()
		if !ok {
			return common.ErrorC(fmt.Errorf("syntax-case: bad syntax"))
		}
		literals = append(literals, id)
	}
	if common.DuplicateIdentifiers(literals...) {
		return common.ErrorC(fmt.Errorf("syntax-case: duplicate literals"))
	}
	var (
		patterns []common.Syntax
		fenders  []common.Syntax
		outputs  []common.Syntax
	)
	for _, clause := range result[common.Symbol("clause")].([]interface{}) {
		var (
			pattern common.Syntax
			fender  common.Syntax
			output  common.Syntax
		)
		if result, ok := PatternSyntaxCaseClause.Match(clause.(common.Syntax)); ok {
			pattern = result[common.Symbol("pattern")].(common.Syntax)
			fender = common.NewSyntax(common.NewWrappedSyntax(common.Boolean(true), nil))
			output = result[common.Symbol("output")].(common.Syntax)
		} else if result, ok := PatternSyntaxCaseClauseWithFender.Match(clause.(common.Syntax)); ok {
			pattern = result[common.Symbol("pattern")].(common.Syntax)
			fender = result[common.Symbol("fender")].(common.Syntax)
			output = result[common.Symbol("output")].(common.Syntax)
		} else {
			return common.ErrorC(fmt.Errorf("syntax-case: bad syntax"))
		}
		patterns = append(patterns, pattern)
		fenders = append(fenders, fender)
		outputs = append(outputs, output)
	}
	return common.CallC(c, SyntaxCaseForm{input, literals, patterns, fenders, outputs})
}

func transformSet(c common.Continuation, args ...common.Datum) (common.Evaluation, error) {
	result, ok := PatternSet.Match(common.NewSyntax(args[0]))
	if !ok {
		return common.ErrorC(fmt.Errorf("set!: bad syntax"))
	}
	id, ok := result[common.Symbol("name")].(common.Syntax).Identifier()
	if !ok {
		return common.ErrorC(fmt.Errorf("set!: bad syntax"))
	}
	form := result[common.Symbol("expression")].(common.Syntax)
	return common.CallC(c, SetForm{id, form})
}

func not(c common.Continuation, args ...common.Datum) (common.Evaluation, error) {
	if len(args) != 1 {
		return common.ErrorC(fmt.Errorf("not: wrong arity"))
	}
	boolean, ok := args[0].(common.Boolean)
	if !ok {
		return common.CallC(c, common.Boolean(false))
	}
	return common.CallC(c, common.Boolean(!boolean))
}

func cons(c common.Continuation, args ...common.Datum) (common.Evaluation, error) {
	if len(args) != 2 {
		return common.ErrorC(fmt.Errorf("cons: wrong arity"))
	}
	return common.CallC(c, common.Pair{args[0], args[1]})
}

func car(c common.Continuation, args ...common.Datum) (common.Evaluation, error) {
	if len(args) != 1 {
		return common.ErrorC(fmt.Errorf("car: wrong arity"))
	}
	pair, ok := args[0].(common.Pair)
	if !ok {
		return common.ErrorC(fmt.Errorf("car: expected pair"))
	}
	return common.CallC(c, pair.First)
}

func cdr(c common.Continuation, args ...common.Datum) (common.Evaluation, error) {
	if len(args) != 1 {
		return common.ErrorC(fmt.Errorf("cdr: wrong arity"))
	}
	pair, ok := args[0].(common.Pair)
	if !ok {
		return common.ErrorC(fmt.Errorf("cdr: expected pair"))
	}
	return common.CallC(c, pair.Rest)
}

func null(c common.Continuation, args ...common.Datum) (common.Evaluation, error) {
	if len(args) != 1 {
		return common.ErrorC(fmt.Errorf("null?: wrong arity"))
	}
	return common.CallC(c, common.Boolean(common.NewSyntax(args[0]).Unwrap() == common.Null))
}

func pair(c common.Continuation, args ...common.Datum) (common.Evaluation, error) {
	if len(args) != 1 {
		return common.ErrorC(fmt.Errorf("pair?: wrong arity"))
	}
	_, ok := args[0].(common.Pair)
	return common.CallC(c, common.Boolean(ok))
}

func proc(c common.Continuation, args ...common.Datum) (common.Evaluation, error) {
	if len(args) != 1 {
		return common.ErrorC(fmt.Errorf("proc?: wrong arity"))
	}
	_, ok := args[0].(common.Procedure)
	return common.CallC(c, common.Boolean(ok))
}

func write(c common.Continuation, args ...common.Datum) (common.Evaluation, error) {
	if len(args) != 1 {
		return common.ErrorC(fmt.Errorf("write: wrong arity"))
	}
	fmt.Print(common.Write(args[0]))
	return common.CallC(c, common.Void)
}

func callWithCurrentContinuation(c common.Continuation, args ...common.Datum) (common.Evaluation, error) {
	if len(args) != 1 {
		return common.ErrorC(fmt.Errorf("call/cc: wrong arity"))
	}
	return common.Apply(c, args[0], common.ContinuationProcedure{c})
}

type stringError string

func (s stringError) Error() string {
	return string(s)
}

func err(c common.Continuation, args ...common.Datum) (common.Evaluation, error) {
	if len(args) != 1 {
		return common.ErrorC(fmt.Errorf("error: wrong arity"))
	}
	message, ok := args[0].(common.String)
	if !ok {
		return common.ErrorC(fmt.Errorf("error: expected string"))
	}
	return common.ErrorC(stringError(message))
}

func eqv(c common.Continuation, args ...common.Datum) (common.Evaluation, error) {
	if len(args) != 2 {
		return common.ErrorC(fmt.Errorf("eqv?: wrong arity"))
	}
	return common.CallC(c, common.Boolean(args[0] == args[1]))
}

func syntaxDatum(c common.Continuation, args ...common.Datum) (common.Evaluation, error) {
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
		if syntax == common.Null {
			return common.Null, nil
		}
		return nil, fmt.Errorf("syntax->datum: expected syntax")
	}
}

func datumSyntax(c common.Continuation, args ...common.Datum) (common.Evaluation, error) {
	if len(args) != 2 {
		return common.ErrorC(fmt.Errorf("datum->syntax: wrong arity"))
	}
	id, ok := common.NewSyntax(args[0]).Identifier()
	if !ok {
		return common.ErrorC(fmt.Errorf("datum->syntax: expected identifier"))
	}
	return common.CallC(c, id.PushOnto(args[1], nil))
}

func identifier(c common.Continuation, args ...common.Datum) (common.Evaluation, error) {
	if len(args) != 1 {
		return common.ErrorC(fmt.Errorf("identifier?: wrong arity"))
	}
	_, ok := common.NewSyntax(args[0]).Identifier()
	return common.CallC(c, common.Boolean(ok))
}

var temporaryIdentifiers int

func generateTemporary() common.WrappedSyntax {
	symbol := common.Symbol(fmt.Sprintf(".%v", temporaryIdentifiers))
	temporaryIdentifiers++
	return common.NewWrappedSyntax(symbol, nil)
}

func generateTemporaries(c common.Continuation, args ...common.Datum) (common.Evaluation, error) {
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

func list(c common.Continuation, args ...common.Datum) (common.Evaluation, error) {
	var list common.Datum = common.Null
	for i := len(args) - 1; i >= 0; i-- {
		list = common.Pair{args[i], list}
	}
	return common.CallC(c, list)
}

func boundIdentifier(c common.Continuation, args ...common.Datum) (common.Evaluation, error) {
	if len(args) != 2 {
		return common.ErrorC(fmt.Errorf("bound-identifier=?: wrong arity"))
	}
	left, ok := common.NewSyntax(args[0]).Identifier()
	if !ok {
		return common.ErrorC(fmt.Errorf("bound-identifier=?: expected identifier"))
	}
	right, ok := common.NewSyntax(args[1]).Identifier()
	if !ok {
		return common.ErrorC(fmt.Errorf("bound-identifier=?: expected identifier"))
	}
	return common.CallC(c, common.Boolean(left.BoundEqual(right)))
}

func freeIdentifier(c common.Continuation, args ...common.Datum) (common.Evaluation, error) {
	if len(args) != 2 {
		return common.ErrorC(fmt.Errorf("free-identifier=?: wrong arity"))
	}
	left, ok := common.NewSyntax(args[0]).Identifier()
	if !ok {
		return common.ErrorC(fmt.Errorf("free-identifier=?: expected identifier"))
	}
	right, ok := common.NewSyntax(args[1]).Identifier()
	if !ok {
		return common.ErrorC(fmt.Errorf("free-identifier=?: expected identifier"))
	}
	return common.CallC(c, common.Boolean(left.FreeEqual(right)))
}
