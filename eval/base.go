package eval

import (
	"fmt"
	"github.com/katsuya94/grime/core"
	"github.com/katsuya94/grime/match"
	"github.com/katsuya94/grime/read"
)

func NewEnvironment() *core.Environment {
	return &core.Environment{map[core.Symbol]core.Binding{
		core.Symbol("quote"):         core.Keyword{core.Procedure(transformQuote)},
		core.Symbol("if"):            core.Keyword{core.Procedure(transformIf)},
		core.Symbol("let*"):          core.Keyword{core.Procedure(transformLetStar)},
		core.Symbol("begin"):         core.Keyword{core.Procedure(transformBegin)},
		core.Symbol("define"):        core.Keyword{core.Procedure(transformDefine)},
		core.Symbol("define-syntax"): core.Keyword{core.Procedure(transformDefineSyntax)},
		core.Symbol("cons"):          core.Variable{core.Procedure(cons)},
	}, true}
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

func transformQuote(env *core.Environment, syntax ...core.Datum) (core.Datum, error) {
	if result, ok, err := match.Match(syntax[0], PatternQuote, nil); err != nil {
		return nil, err
	} else if !ok {
		return nil, fmt.Errorf("quote: bad syntax")
	} else {
		return core.Quote{result[core.Symbol("datum")]}, nil
	}
}

func transformIf(env *core.Environment, syntax ...core.Datum) (core.Datum, error) {
	if result, ok, err := match.Match(syntax[0], PatternIf, nil); err != nil {
		return nil, err
	} else if !ok {
		return nil, fmt.Errorf("if: bad syntax")
	} else {
		conditionExpression, err := ExpandMacro(env, result[core.Symbol("condition")])
		if err != nil {
			return nil, err
		}
		thenExpression, err := ExpandMacro(env, result[core.Symbol("then")])
		if err != nil {
			return nil, err
		}
		elseExpression, err := ExpandMacro(env, result[core.Symbol("else")])
		if err != nil {
			return nil, err
		}
		return core.If{conditionExpression, thenExpression, elseExpression}, nil
	}
}

func transformLetStar(env *core.Environment, syntax ...core.Datum) (core.Datum, error) {
	if result, ok, err := match.Match(syntax[0], PatternLetStar, nil); err != nil {
		return nil, err
	} else if !ok {
		return nil, fmt.Errorf("let: bad syntax")
	} else {
		var names []core.Symbol
		for _, s := range result[core.Symbol("name")].([]interface{}) {
			if symbol, ok := s.(core.Symbol); ok {
				names = append(names, symbol)
			} else {
				return nil, fmt.Errorf("let: bad syntax")
			}
		}
		var valueExpressions []core.Datum
		for _, s := range result[core.Symbol("value")].([]interface{}) {
			if expression, err := ExpandMacro(env, s); err != nil {
				return nil, err
			} else {
				valueExpressions = append(valueExpressions, expression)
			}
		}
		var forms []core.Datum
		for _, form := range result[core.Symbol("body")].([]interface{}) {
			forms = append(forms, form)
		}
		expression, err := ExpandBody(env, forms)
		if err != nil {
			return nil, err
		}
		for i := len(names) - 1; i >= 0; i-- {
			expression = core.Let{names[i], valueExpressions[i], expression}
		}
		return expression, nil
	}
}

func transformBegin(env *core.Environment, syntax ...core.Datum) (core.Datum, error) {
	result, ok, err := match.Match(syntax[0], PatternBegin, nil)
	if err != nil {
		return nil, err
	} else if !ok {
		return nil, fmt.Errorf("begin: bad syntax")
	}
	if env.ExpressionContext {
		var expressions []core.Datum
		for _, form := range result[core.Symbol("body")].([]interface{}) {
			if expression, err := ExpandMacro(env, form); err != nil {
				return nil, err
			} else {
				expressions = append(expressions, expression)
			}
		}
		return core.Begin{expressions}, nil
	} else {
		var forms []core.Datum
		for _, form := range result[core.Symbol("body")].([]interface{}) {
			forms = append(forms, form)
		}
		return core.Begin{forms}, nil
	}
}

func transformDefine(env *core.Environment, syntax ...core.Datum) (core.Datum, error) {
	if _, ok, err := match.Match(syntax[0], PatternDefineProcedure, nil); err != nil {
		return nil, err
	} else if ok {
		return nil, fmt.Errorf("define: define procedure not implemented")
	}
	// TODO implement other define forms
	if result, ok, err := match.Match(syntax[0], PatternDefine, nil); err != nil {
		return nil, err
	} else if ok {
		name, ok := result[core.Symbol("name")].(core.Symbol)
		if !ok {
			return nil, fmt.Errorf("define: bad syntax")
		}
		return core.Define{name, result[core.Symbol("value")]}, nil
	}
	return nil, fmt.Errorf("define: bad syntax")
}

func transformDefineSyntax(env *core.Environment, syntax ...core.Datum) (core.Datum, error) {
	if result, ok, err := match.Match(syntax[0], PatternDefineSyntax, nil); err != nil {
		return nil, err
	} else if !ok {
		return nil, fmt.Errorf("define-syntax: bad syntax")
	} else {
		name, ok := result[core.Symbol("name")].(core.Symbol)
		if !ok {
			return nil, fmt.Errorf("define-syntax: bad syntax")
		}
		expression, err := ExpandMacro(env, result[core.Symbol("value")])
		if err != nil {
			return nil, err
		}
		return core.DefineSyntax{name, expression}, nil
	}
}

func cons(env *core.Environment, args ...core.Datum) (core.Datum, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("cons: wrong arity")
	}
	return core.Pair{args[0], args[1]}, nil
}
