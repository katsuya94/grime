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
	common.Symbol("quote"):         common.Keyword{common.Procedure(transformQuote)},
	common.Symbol("if"):            common.Keyword{common.Procedure(transformIf)},
	common.Symbol("let*"):          common.Keyword{common.Procedure(transformLetStar)},
	common.Symbol("begin"):         common.Keyword{common.Procedure(transformBegin)},
	common.Symbol("define"):        common.Keyword{common.Procedure(transformDefine)},
	common.Symbol("define-syntax"): common.Keyword{common.Procedure(transformDefineSyntax)},
	common.Symbol("cons"):          common.Variable{common.Procedure(cons)},
	common.Symbol("write"):         common.Variable{common.Procedure(write)},
}

var (
	PatternQuote           = read.MustReadString("(quote datum)")[0]
	PatternIf              = read.MustReadString("(if condition then else)")[0]
	PatternLetStar         = read.MustReadString("(let* ((name value) ...) body ...)")[0]
	PatternBegin           = read.MustReadString("(begin body ...)")[0]
	PatternDefineProcedure = read.MustReadString("(define (name formals ...) body0 body ...)")[0]
	PatternDefine          = read.MustReadString("(define name value)")[0]
	PatternDefineSyntax    = read.MustReadString("(define-syntax name value)")[0]
)

func transformQuote(env *common.Environment, syntax ...common.Datum) (common.Datum, error) {
	if result, ok, err := util.Match(syntax[0], PatternQuote, nil); err != nil {
		return nil, err
	} else if !ok {
		return nil, fmt.Errorf("quote: bad syntax")
	} else {
		return common.Quote{result[common.Symbol("datum")]}, nil
	}
}

func transformIf(env *common.Environment, syntax ...common.Datum) (common.Datum, error) {
	if result, ok, err := util.Match(syntax[0], PatternIf, nil); err != nil {
		return nil, err
	} else if !ok {
		return nil, fmt.Errorf("if: bad syntax")
	} else {
		conditionExpression, err := eval.ExpandMacro(env, result[common.Symbol("condition")])
		if err != nil {
			return nil, err
		}
		thenExpression, err := eval.ExpandMacro(env, result[common.Symbol("then")])
		if err != nil {
			return nil, err
		}
		elseExpression, err := eval.ExpandMacro(env, result[common.Symbol("else")])
		if err != nil {
			return nil, err
		}
		return common.If{conditionExpression, thenExpression, elseExpression}, nil
	}
}

func transformLetStar(env *common.Environment, syntax ...common.Datum) (common.Datum, error) {
	if result, ok, err := util.Match(syntax[0], PatternLetStar, nil); err != nil {
		return nil, err
	} else if !ok {
		return nil, fmt.Errorf("let: bad syntax")
	} else {
		var names []common.Symbol
		for _, s := range result[common.Symbol("name")].([]interface{}) {
			if symbol, ok := s.(common.Symbol); ok {
				names = append(names, symbol)
			} else {
				return nil, fmt.Errorf("let: bad syntax")
			}
		}
		var valueExpressions []common.Datum
		for _, s := range result[common.Symbol("value")].([]interface{}) {
			if expression, err := eval.ExpandMacro(env, s); err != nil {
				return nil, err
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
			return nil, err
		}
		for i := len(names) - 1; i >= 0; i-- {
			expression = common.Let{names[i], valueExpressions[i], expression}
		}
		return expression, nil
	}
}

func transformBegin(env *common.Environment, syntax ...common.Datum) (common.Datum, error) {
	result, ok, err := util.Match(syntax[0], PatternBegin, nil)
	if err != nil {
		return nil, err
	} else if !ok {
		return nil, fmt.Errorf("begin: bad syntax")
	}
	if env.ExpressionContext {
		forms := result[common.Symbol("body")].([]interface{})
		if len(forms) == 0 {
			return nil, fmt.Errorf("begin: bad syntax")
		}
		var expressions []common.Datum
		for _, form := range forms {
			if expression, err := eval.ExpandMacro(env, form); err != nil {
				return nil, err
			} else {
				expressions = append(expressions, expression)
			}
		}
		return common.Begin{expressions}, nil
	} else {
		var forms []common.Datum
		for _, form := range result[common.Symbol("body")].([]interface{}) {
			forms = append(forms, form)
		}
		return common.Begin{forms}, nil
	}
}

func transformDefine(env *common.Environment, syntax ...common.Datum) (common.Datum, error) {
	if _, ok, err := util.Match(syntax[0], PatternDefineProcedure, nil); err != nil {
		return nil, err
	} else if ok {
		return nil, fmt.Errorf("define: define procedure not implemented")
	}
	// TODO implement other define forms
	if result, ok, err := util.Match(syntax[0], PatternDefine, nil); err != nil {
		return nil, err
	} else if ok {
		name, ok := result[common.Symbol("name")].(common.Symbol)
		if !ok {
			return nil, fmt.Errorf("define: bad syntax")
		}
		return common.Define{name, result[common.Symbol("value")]}, nil
	}
	return nil, fmt.Errorf("define: bad syntax")
}

func transformDefineSyntax(env *common.Environment, syntax ...common.Datum) (common.Datum, error) {
	if result, ok, err := util.Match(syntax[0], PatternDefineSyntax, nil); err != nil {
		return nil, err
	} else if !ok {
		return nil, fmt.Errorf("define-syntax: bad syntax")
	} else {
		name, ok := result[common.Symbol("name")].(common.Symbol)
		if !ok {
			return nil, fmt.Errorf("define-syntax: bad syntax")
		}
		expression, err := eval.ExpandMacro(env, result[common.Symbol("value")])
		if err != nil {
			return nil, err
		}
		return common.DefineSyntax{name, expression}, nil
	}
}

func cons(env *common.Environment, args ...common.Datum) (common.Datum, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("cons: wrong arity")
	}
	return common.Pair{args[0], args[1]}, nil
}

func write(env *common.Environment, args ...common.Datum) (common.Datum, error) {
	if len(args) != 1 {
		return nil, fmt.Errorf("write: wrong arity")
	}
	fmt.Print(util.Write(args[0]))
	return common.Void, nil
}
