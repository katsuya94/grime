package eval

import (
	"fmt"
	"github.com/katsuya94/grime/core"
	"github.com/katsuya94/grime/match"
	"github.com/katsuya94/grime/read"
)

func NewEnvironment() *core.Environment {
	return &core.Environment{map[core.Symbol]core.Binding{
		core.Symbol("quote"): core.Keyword(core.Procedure(transformQuote)),
		core.Symbol("if"):    core.Keyword(core.Procedure(transformIf)),
	}}
}

var (
	PatternQuote = read.MustReadString("(quote datum)")[0]
	PatternIf    = read.MustReadString("(if condition then else)")[0]
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
