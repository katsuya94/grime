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

type BodyCompiler func(compiler Compiler, forms []common.Datum, defined []common.WrappedSyntax) (common.Expression, []common.WrappedSyntax, error)
type ExpressionCompiler func(compiler Compiler, form common.Datum) (common.Expression, error)
type Expander func(compiler Compiler, form common.Datum) (common.Datum, bool, error)

type Compiler struct {
	BodyCompiler       BodyCompiler
	ExpressionCompiler ExpressionCompiler
	Expander           Expander
	Phase              int
}

func NewCompiler() Compiler {
	return Compiler{
		BodyCompiler:       BodyCompile,
		ExpressionCompiler: ExpressionCompile,
		Expander:           Expand,
		Phase:              0,
	}
}

func (compiler Compiler) BodyCompile(forms []common.Datum, defined []common.WrappedSyntax) (common.Expression, []common.WrappedSyntax, error) {
	return compiler.BodyCompiler(compiler, forms, defined)
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

func (compiler Compiler) Next() Compiler {
	compiler.Phase += 1
	return compiler
}

func Compile(body common.WrappedSyntax) (common.Expression, []common.WrappedSyntax, error) {
	defined := body.DefinedAt(0)
	var forms []common.Datum
	for {
		_, ok := body.Datum().(common.Pair)
		if !ok {
			break
		}
		pair := body.PushDown().(common.Pair)
		forms = append(forms, pair.First)
		body = pair.Rest.(common.WrappedSyntax)
	}
	return NewCompiler().BodyCompile(forms, defined)
}
