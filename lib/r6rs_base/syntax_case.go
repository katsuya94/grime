package r6rs_base

import (
	"fmt"

	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/lib/r6rs"
	"github.com/katsuya94/grime/read"
)

// TODO: move syntax-case to its own library

var syntaxCaseTransformer = r6rs.NewCoreTransformer(transformSyntaxCase)
var (
	patternSyntaxCase                 = common.MustCompileSimplePattern(read.MustReadDatum("(syntax-case input (literal ...) clause ...)"))
	patternSyntaxCaseClause           = common.MustCompileSimplePattern(read.MustReadDatum("(pattern output)"))
	patternSyntaxCaseClauseWithFender = common.MustCompileSimplePattern(read.MustReadDatum("(pattern fender output)"))
)

func transformSyntaxCase(ctx common.ExpansionContext, syntax common.Syntax, mark *common.M) (common.CoreForm, error) {
	result, ok := patternSyntaxCase.Match(syntax)
	if !ok {
		return nil, fmt.Errorf("%v: bad syntax", syntaxKeywordForErrMsg(syntax))
	}
	input := result[common.Symbol("input")].(common.Syntax)
	inputCoreForm, err := ctx.Expander.Expand(ctx, input)
	if err != nil {
		return nil, err
	}
	var literals []common.Identifier
	for _, literal := range result[common.Symbol("literal")].([]interface{}) {
		id, ok := literal.(common.Syntax).Identifier()
		if !ok {
			return nil, fmt.Errorf("%v: bad syntax", syntaxKeywordForErrMsg(syntax))
		}
		literals = append(literals, id)
	}
	literalScope := common.NewScope()
	patternEnv := ctx.Env
	for _, literal := range literals {
		_, binding := common.Bind(literal, literalScope)
		(&patternEnv).Extend(binding, common.NewPatternLiteral(literal))
	}
	if common.DuplicateIdentifiers(literals...) {
		return nil, fmt.Errorf("%v: duplicate literals", syntaxKeywordForErrMsg(syntax))
	}
	clauses := make([]syntaxCaseFormClause, len(result[common.Symbol("clause")].([]interface{})))
	for i, clause := range result[common.Symbol("clause")].([]interface{}) {
		var (
			pattern common.Syntax
			fender  common.Syntax
			output  common.Syntax
		)
		if result, ok := patternSyntaxCaseClause.Match(clause.(common.Syntax)); ok {
			pattern = result[common.Symbol("pattern")].(common.Syntax)
			fender = common.NewSyntax(common.NewWrappedSyntax(common.Boolean(true), nil).Mark(mark))
			output = result[common.Symbol("output")].(common.Syntax)
		} else if result, ok := patternSyntaxCaseClauseWithFender.Match(clause.(common.Syntax)); ok {
			pattern = result[common.Symbol("pattern")].(common.Syntax)
			fender = result[common.Symbol("fender")].(common.Syntax)
			output = result[common.Symbol("output")].(common.Syntax)
		} else {
			return nil, fmt.Errorf("%v: bad syntax", syntaxKeywordForErrMsg(syntax))
		}
		clauseScope := common.NewScope()
		compiledPattern, patternVariableInfos, err := common.CompilePattern(pattern, clauseScope, patternEnv)
		if err != nil {
			return nil, err
		}
		patternVariableIds := make([]common.Identifier, len(patternVariableInfos))
		clauseCtx := common.ExpansionContext{Expander: ctx.Expander, Env: ctx.Env}
		for i, patternVariableInfo := range patternVariableInfos {
			patternVariableIds[i] = patternVariableInfo.Binding.Identifier()
			(&clauseCtx.Env).Extend(patternVariableInfo.Binding, common.NewPatternVariable(patternVariableInfo.Nesting))
		}
		fender = fender.Push(clauseScope)
		fenderCoreForm, err := clauseCtx.Expander.Expand(clauseCtx, fender)
		if err != nil {
			return nil, err
		}
		output = output.Push(clauseScope)
		outputCoreForm, err := clauseCtx.Expander.Expand(clauseCtx, output)
		if err != nil {
			return nil, err
		}
		clauses[i] = syntaxCaseFormClause{compiledPattern, patternVariableIds, fenderCoreForm, outputCoreForm}
	}
	return syntaxCaseForm{inputCoreForm, clauses}, nil
}

type syntaxCaseFormClause struct {
	pattern            common.Pattern
	patternVariableIds []common.Identifier
	fender             common.CoreForm
	output             common.CoreForm
}

type syntaxCaseForm struct {
	input   common.CoreForm
	clauses []syntaxCaseFormClause
}

func (f syntaxCaseForm) CpsTransform(ctx *common.CpsTransformContext) (common.Expression, error) {
	input, err := f.input.CpsTransform(ctx)
	if err != nil {
		return nil, err
	}
	clauses := make([]syntaxCaseClause, len(f.clauses))
	for i, clause := range f.clauses {
		// TODO: should each clause have its own evaluation context?
		patternVariableIndexes := make([]bindingIndex, len(clause.patternVariableIds))
		for i, patternVariableId := range clause.patternVariableIds {
			patternVariableIndexes[i] = bindingIndex{patternVariableId.Binding(), ctx.Add(patternVariableId)}
		}
		fender, err := clause.fender.CpsTransform(ctx)
		if err != nil {
			return nil, err
		}
		output, err := clause.output.CpsTransform(ctx)
		if err != nil {
			return nil, err
		}
		clauses[i] = syntaxCaseClause{clause.pattern, patternVariableIndexes, fender, output}
	}
	return syntaxCase{input, clauses}, nil
}

type bindingIndex struct {
	binding *common.Binding
	index   int
}

type syntaxCaseClause struct {
	pattern                common.Pattern
	patternVariableIndexes []bindingIndex
	fender                 common.Expression
	output                 common.Expression
}

type syntaxCase struct {
	input   common.Expression
	clauses []syntaxCaseClause
}

func (e syntaxCase) Evaluate(ctx common.EvaluationContext, c common.Continuation) (common.Evaluation, error) {
	return common.EvalC(
		ctx,
		syntaxCaseInputEvaluated{ctx, c, e.clauses},
		e.input,
	)
}

type syntaxCaseInputEvaluated struct {
	ctx          common.EvaluationContext
	continuation common.Continuation
	clauses      []syntaxCaseClause
}

func (c syntaxCaseInputEvaluated) Call(d common.Datum) (common.Evaluation, error) {
	return syntaxCaseMatch(c.ctx, c.continuation, d, c.clauses)
}

type syntaxCaseFenderEvaluated struct {
	ctx          common.EvaluationContext
	continuation common.Continuation
	input        common.Datum
	output       common.Expression
	clauses      []syntaxCaseClause
}

func (c syntaxCaseFenderEvaluated) Call(d common.Datum) (common.Evaluation, error) {
	if d == common.Boolean(false) {
		return syntaxCaseMatch(c.ctx, c.continuation, c.input, c.clauses)
	}
	return common.EvalC(c.ctx, c.continuation, c.output)
}

func syntaxCaseMatch(ctx common.EvaluationContext, c common.Continuation, input common.Datum, clauses []syntaxCaseClause) (common.Evaluation, error) {
	syntax := common.NewSyntax(input)
	for i, clause := range clauses {
		result, ok := clause.pattern.Match(syntax)
		if ok {
			for _, bindingIndex := range clause.patternVariableIndexes {
				ctx.Get(bindingIndex.index).Set(result[bindingIndex.binding])
			}
			return common.EvalC(
				ctx,
				syntaxCaseFenderEvaluated{
					ctx,
					c,
					input,
					clauses[i].output,
					clauses[i+1:],
				},
				clauses[i].fender,
			)
		}
	}
	return nil, fmt.Errorf("bad syntax %v at %v", common.Write(syntax.Datum()), syntax.SourceLocation())
}
