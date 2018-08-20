package eval

import (
	"fmt"
	"github.com/katsuya94/grime/core"
	"github.com/katsuya94/grime/match"
	"github.com/katsuya94/grime/read"
)

func NewEnvironment() *core.Environment {
	return &core.Environment{map[core.Symbol]core.Binding{
		core.Symbol("quote"): core.Keyword{core.Procedure(transformQuote)},
		core.Symbol("if"):    core.Keyword{core.Procedure(transformIf)},
		core.Symbol("let*"):  core.Keyword{core.Procedure(transformLetStar)},
		core.Symbol("cons"):  core.Variable{core.Procedure(cons)},
	}}
}

var (
	PatternQuote = read.MustReadString("(quote datum)")[0]
	PatternIf    = read.MustReadString("(if condition then else)")[0]
	PatternLet   = read.MustReadString("(let* ((name value) ...) body0 body ...)")[0]
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
	if result, ok, err := match.Match(syntax[0], PatternLet, nil); err != nil {
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
		forms := []core.Datum{result[core.Symbol("body0")]}
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

func cons(env *core.Environment, args ...core.Datum) (core.Datum, error) {
	if len(args) != 2 {
		return nil, fmt.Errorf("cons: wrong arity")
	}
	return core.Pair{args[0], args[1]}, nil
}
