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

type BodyCompiler func(compiler Compiler, forms []common.Syntax, scope common.Scope, frameTemplate *common.FrameTemplate) (common.Expression, error)
type ExpressionCompiler func(compiler Compiler, form common.Syntax, frameTemplate *common.FrameTemplate) (common.Expression, error)
type Expander func(compiler Compiler, form common.Syntax) (common.Syntax, bool, error)

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

func (compiler Compiler) BodyCompile(forms []common.Syntax, scope common.Scope, frameTemplate *common.FrameTemplate) (common.Expression, error) {
	return compiler.BodyCompiler(compiler, forms, scope, frameTemplate)
}

func (compiler Compiler) ExpressionCompile(form common.Syntax, frameTemplate *common.FrameTemplate) (common.Expression, error) {
	return compiler.ExpressionCompiler(compiler, form, frameTemplate)
}

func (compiler Compiler) Expand(form common.Syntax) (common.Syntax, bool, error) {
	return compiler.Expander(compiler, form)
}

func (compiler Compiler) ExpandCompletely(form common.Syntax) (common.Syntax, error) {
	for {
		expanded, ok, err := compiler.Expand(form)
		if err != nil {
			return common.Syntax{}, err
		} else if !ok {
			return form, nil
		}
		form = expanded
	}
}

func Compile(body common.Syntax, scope common.Scope) (common.Expression, common.FrameTemplate, error) {
	scope = common.NewProxyScope(scope)
	body = body.Push(scope, common.LEXICAL)
	var forms []common.Syntax
	for {
		pair, ok := body.Pair()
		if !ok {
			break
		}
		first := common.NewSyntax(pair.First)
		rest := common.NewSyntax(pair.Rest)
		forms = append(forms, first)
		body = rest
	}
	frameTemplate := common.NewFrameTemplate()
	expression, err := NewCompiler().BodyCompile(forms, scope, &frameTemplate)
	if err != nil {
		return nil, common.FrameTemplate{}, err
	}
	return expression, frameTemplate, nil
}
