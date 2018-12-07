package core

import (
	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/util"
)

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
	err := util.Each(body.Datum(), func(datum common.Datum) error {
		forms = append(forms, body.PushOnto(datum))
		return nil
	})
	if err != nil {
		return nil, nil, err
	}
	return NewCompiler().BodyCompile(forms, defined)
}
