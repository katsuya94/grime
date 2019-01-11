package core

import (
	"fmt"

	"github.com/katsuya94/grime/common"
)

type ExpressionCompileError struct {
	err            error
	context        string
	sourceLocation common.SourceLocation
}

func (err ExpressionCompileError) Error() string {
	if (err.sourceLocation == common.SourceLocation{}) {
		return err.err.Error()
	}
	return fmt.Sprintf("in %v expanded from %v: %v", err.context, err.sourceLocation, err.err)
}

type BodyCompiler func(compiler Compiler, forms []common.Datum, scope common.Scope) (common.Expression, error)
type ExpressionCompiler func(compiler Compiler, form common.Datum) (common.Expression, error)
type Expander func(compiler Compiler, form common.Datum) (common.Datum, bool, error)

type Compiler struct {
	BodyCompiler       BodyCompiler
	ExpressionCompiler ExpressionCompiler
	Expander           Expander
}

func NewCompiler() Compiler {
	return Compiler{
		BodyCompiler:       BodyCompile,
		ExpressionCompiler: ExpressionCompile,
		Expander:           Expand,
	}
}

func (compiler Compiler) BodyCompile(forms []common.Datum, scope common.Scope) (common.Expression, error) {
	return compiler.BodyCompiler(compiler, forms, scope)
}

func (compiler Compiler) ExpressionCompile(form common.Datum) (common.Expression, error) {
	return compiler.ExpressionCompiler(compiler, form)
}

func (compiler Compiler) Expand(form common.Datum) (common.Datum, bool, error) {
	return compiler.Expander(compiler, form)
}

func (compiler Compiler) ExpandCompletely(form common.Datum) (common.Datum, error) {
	for {
		expanded, ok, err := compiler.Expand(form)
		if err != nil {
			return nil, err
		} else if !ok {
			return form, nil
		}
		form = expanded
	}
}

func Compile(body common.Syntax, scope common.Scope) (common.Expression, error) {
	scope = common.NewProxyScope(scope)
	body = body.Push(scope, common.LEXICAL)
	var forms []common.Datum
	for {
		pair, ok := body.Pair()
		if !ok {
			break
		}
		first := common.NewSyntax(pair.First)
		rest := common.NewSyntax(pair.Rest)
		forms = append(forms, first.Form())
		body = rest
	}
	return NewCompiler().BodyCompile(forms, scope)
}
