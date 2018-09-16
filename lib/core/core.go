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
	common.Symbol("if"):            common.Keyword{common.Function(transformIf)},
	common.Symbol("let*"):          common.Keyword{common.Function(transformLetStar)},
	common.Symbol("begin"):         common.Keyword{common.Function(transformBegin)},
	common.Symbol("lambda"):        common.Keyword{common.Function(transformLambda)},
	common.Symbol("define"):        common.Keyword{common.Function(transformDefine)},
	common.Symbol("define-syntax"): common.Keyword{common.Function(transformDefineSyntax)},
	common.Symbol("cons"):          common.Variable{common.Function(cons)},
	common.Symbol("car"):           common.Variable{common.Function(car)},
	common.Symbol("cdr"):           common.Variable{common.Function(cdr)},
	common.Symbol("null?"):         common.Variable{common.Function(null)},
	common.Symbol("write"):         common.Variable{common.Function(write)},
	common.Symbol("call/cc"):       common.Variable{common.Function(callWithCurrentContinuation)},
}

var (
	PatternQuote          = read.MustReadString("(quote datum)")[0]
	PatternIf             = read.MustReadString("(if condition then else)")[0]
	PatternLetStar        = read.MustReadString("(let* ((name value) ...) body ...)")[0]
	PatternBegin          = read.MustReadString("(begin body ...)")[0]
	PatternLambda         = read.MustReadString("(lambda (formals ...) body ...)")[0]
	PatternDefineFunction = read.MustReadString("(define (name formals ...) body ...)")[0]
	PatternDefine         = read.MustReadString("(define name value)")[0]
	PatternDefineSyntax   = read.MustReadString("(define-syntax name value)")[0]
)

func transformQuote(env common.Environment, syntax ...common.Datum) (common.EvaluationResult, error) {
	if result, ok, err := util.Match(syntax[0], PatternQuote, nil); err != nil {
		return common.ErrorC(err)
	} else if !ok {
		return common.ErrorC(fmt.Errorf("quote: bad syntax"))
	} else {
		return common.CallC(env, common.Quote{result[common.Symbol("datum")]})
	}
}

func transformIf(env common.Environment, syntax ...common.Datum) (common.EvaluationResult, error) {
	if result, ok, err := util.Match(syntax[0], PatternIf, nil); err != nil {
		return common.ErrorC(err)
	} else if !ok {
		return common.ErrorC(fmt.Errorf("if: bad syntax"))
	} else {
		conditionExpression, err := eval.ExpandMacro(env, result[common.Symbol("condition")])
		if err != nil {
			return common.ErrorC(err)
		}
		thenExpression, err := eval.ExpandMacro(env, result[common.Symbol("then")])
		if err != nil {
			return common.ErrorC(err)
		}
		elseExpression, err := eval.ExpandMacro(env, result[common.Symbol("else")])
		if err != nil {
			return common.ErrorC(err)
		}
		return common.CallC(env, common.If{conditionExpression, thenExpression, elseExpression})
	}
}

func transformLetStar(env common.Environment, syntax ...common.Datum) (common.EvaluationResult, error) {
	if result, ok, err := util.Match(syntax[0], PatternLetStar, nil); err != nil {
		return common.ErrorC(err)
	} else if !ok {
		return common.ErrorC(fmt.Errorf("let: bad syntax"))
	} else {
		var names []common.Symbol
		for _, s := range result[common.Symbol("name")].([]interface{}) {
			if symbol, ok := s.(common.Symbol); ok {
				names = append(names, symbol)
			} else {
				return common.ErrorC(fmt.Errorf("let: bad syntax"))
			}
		}
		var valueExpressions []common.Datum
		for _, s := range result[common.Symbol("value")].([]interface{}) {
			if expression, err := eval.ExpandMacro(env, s); err != nil {
				return common.ErrorC(err)
			} else {
				valueExpressions = append(valueExpressions, expression)
			}
		}
		var forms []common.Datum
		for _, form := range result[common.Symbol("body")].([]interface{}) {
			forms = append(forms, form)
		}
		expression, err := eval.ExpandBody(env, forms)
		if err != nil {
			return common.ErrorC(err)
		}
		for i := len(names) - 1; i >= 0; i-- {
			expression = common.Let{names[i], valueExpressions[i], expression}
		}
		return common.CallC(env, expression)
	}
}

func transformBegin(env common.Environment, syntax ...common.Datum) (common.EvaluationResult, error) {
	result, ok, err := util.Match(syntax[0], PatternBegin, nil)
	if err != nil {
		return common.ErrorC(err)
	} else if !ok {
		return common.ErrorC(fmt.Errorf("begin: bad syntax"))
	}
	if env.ExpressionContext() {
		forms := result[common.Symbol("body")].([]interface{})
		if len(forms) == 0 {
			return common.ErrorC(fmt.Errorf("begin: bad syntax"))
		}
		var expressions []common.Datum
		for _, form := range forms {
			if expression, err := eval.ExpandMacro(env, form); err != nil {
				return common.ErrorC(err)
			} else {
				expressions = append(expressions, expression)
			}
		}
		return common.CallC(env, common.Begin{expressions})
	} else {
		var forms []common.Datum
		for _, form := range result[common.Symbol("body")].([]interface{}) {
			forms = append(forms, form)
		}
		return common.CallC(env, common.Begin{forms})
	}
}

func transformLambda(env common.Environment, syntax ...common.Datum) (common.EvaluationResult, error) {
	if result, ok, err := util.Match(syntax[0], PatternLambda, nil); err != nil {
		return common.ErrorC(err)
	} else if ok {
		lambda, err := makeLambdaFromResult(env, result)
		if err != nil {
			return common.ErrorC(err)
		}
		return common.CallC(env, lambda)
	}
	return common.ErrorC(fmt.Errorf("lambda: bad syntax"))
}

func transformDefine(env common.Environment, syntax ...common.Datum) (common.EvaluationResult, error) {
	var (
		name  common.Symbol
		value common.Datum
	)
	// TODO implement other define forms
	// TODO implement define procedure with syntax transformation
	if result, ok, err := util.Match(syntax[0], PatternDefineFunction, nil); err != nil {
		return common.ErrorC(err)
	} else if ok {
		name, ok = result[common.Symbol("name")].(common.Symbol)
		if !ok {
			return common.ErrorC(fmt.Errorf("define: bad syntax"))
		}
		lambda, err := makeLambdaFromResult(env, result)
		if err != nil {
			return common.ErrorC(err)
		}
		value = lambda
	} else if result, ok, err := util.Match(syntax[0], PatternDefine, nil); err != nil {
		return common.ErrorC(err)
	} else if ok {
		name, ok = result[common.Symbol("name")].(common.Symbol)
		if !ok {
			return common.ErrorC(fmt.Errorf("define: bad syntax"))
		}
		value = result[common.Symbol("value")]
	} else {
		return common.ErrorC(fmt.Errorf("define: bad syntax"))
	}
	return common.CallC(env, common.Define{name, value})
}

func makeLambdaFromResult(env common.Environment, result map[common.Symbol]interface{}) (common.Lambda, error) {
	var formals []common.Symbol
	for _, s := range result[common.Symbol("formals")].([]interface{}) {
		if formal, ok := s.(common.Symbol); ok {
			formals = append(formals, formal)
		} else {
			return common.Lambda{}, fmt.Errorf("lambda: bad syntax")
		}
	}
	var forms []common.Datum
	for _, form := range result[common.Symbol("body")].([]interface{}) {
		forms = append(forms, form)
	}
	expression, err := eval.ExpandBody(env, forms)
	if err != nil {
		return common.Lambda{}, err
	}
	return common.Lambda{formals, expression}, nil
}

func transformDefineSyntax(env common.Environment, syntax ...common.Datum) (common.EvaluationResult, error) {
	if result, ok, err := util.Match(syntax[0], PatternDefineSyntax, nil); err != nil {
		return common.ErrorC(err)
	} else if !ok {
		return common.ErrorC(fmt.Errorf("define-syntax: bad syntax"))
	} else {
		name, ok := result[common.Symbol("name")].(common.Symbol)
		if !ok {
			return common.ErrorC(fmt.Errorf("define-syntax: bad syntax"))
		}
		expression, err := eval.ExpandMacro(env, result[common.Symbol("value")])
		if err != nil {
			return common.ErrorC(err)
		}
		return common.CallC(env, common.DefineSyntax{name, expression})
	}
}

func cons(env common.Environment, args ...common.Datum) (common.EvaluationResult, error) {
	if len(args) != 2 {
		return common.ErrorC(fmt.Errorf("cons: wrong arity"))
	}
	return common.CallC(env, common.Pair{args[0], args[1]})
}

func car(env common.Environment, args ...common.Datum) (common.EvaluationResult, error) {
	if len(args) != 1 {
		return common.ErrorC(fmt.Errorf("car: wrong arity"))
	}
	pair, ok := args[0].(common.Pair)
	if !ok {
		return common.ErrorC(fmt.Errorf("car: expected pair"))
	}
	return common.CallC(env, pair.First)
}

func cdr(env common.Environment, args ...common.Datum) (common.EvaluationResult, error) {
	if len(args) != 1 {
		return common.ErrorC(fmt.Errorf("cdr: wrong arity"))
	}
	pair, ok := args[0].(common.Pair)
	if !ok {
		return common.ErrorC(fmt.Errorf("cdr: expected pair"))
	}
	return common.CallC(env, pair.Rest)
}

func null(env common.Environment, args ...common.Datum) (common.EvaluationResult, error) {
	if len(args) != 1 {
		return common.ErrorC(fmt.Errorf("null?: wrong arity"))
	}
	return common.CallC(env, common.Boolean(args[0] == nil))
}

func write(env common.Environment, args ...common.Datum) (common.EvaluationResult, error) {
	if len(args) != 1 {
		return common.ErrorC(fmt.Errorf("write: wrong arity"))
	}
	fmt.Print(util.Write(args[0]))
	return common.CallC(env, common.Void)
}

func callWithCurrentContinuation(env common.Environment, args ...common.Datum) (common.EvaluationResult, error) {
	if len(args) != 1 {
		return common.ErrorC(fmt.Errorf("call/cc: wrong arity"))
	}
	return eval.Apply(env, args[0], common.ContinuationProcedure{env.Continuation()})
}
