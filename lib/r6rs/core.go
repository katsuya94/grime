package r6rs

import (
	"fmt"

	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/read"
)

var CoreScope = map[common.Symbol]common.Function{
	common.Symbol("#%literal"):   transformLiteral,
	common.Symbol("#%reference"): transformReference,
	common.Symbol("#%lambda"):    transformLambda,
}

type EqualityContext interface{}

type CoreForm interface {
	Unexpand() common.Datum
	Equal(EqualityContext, CoreForm) bool
}

type LiteralForm struct {
	Datum common.Datum
}

var patternLiteral = common.MustCompileSimplePattern(read.MustReadDatum("(#%literal datum)"))

func transformLiteral(c common.Continuation, args ...common.Datum) (common.Evaluation, error) {
	result, ok := patternLiteral.Match(common.NewSyntax(args[0]))
	if !ok {
		return common.ErrorC(fmt.Errorf("#%%literal: bad syntax"))
	}
	return common.CallC(c, LiteralForm{result[common.Symbol("datum")].(common.Syntax).Unwrap()})
}

func (f LiteralForm) Mark(m *common.M) common.Marker {
	return f
}

func (f LiteralForm) Unexpand() common.Datum {
	return common.Pair{common.Symbol("#%literal"), common.Pair{f.Datum, common.Null}}
}

type ReferenceForm struct {
	Identifier common.Identifier
}

var referenceLiteral = common.MustCompileSimplePattern(read.MustReadDatum("(#%reference id)"))

func transformReference(c common.Continuation, args ...common.Datum) (common.Evaluation, error) {
	result, ok := referenceLiteral.Match(common.NewSyntax(args[0]))
	if !ok {
		return common.ErrorC(fmt.Errorf("#%%literal: bad syntax"))
	}
	id, ok := result[common.Symbol("datum")].(common.Syntax).Identifier()
	if !ok {
		return common.ErrorC(fmt.Errorf("#%%literal: bad syntax"))
	}
	return common.CallC(c, ReferenceForm{id})
}

func (f ReferenceForm) Mark(m *common.M) common.Marker {
	return ReferenceForm{
		f.Identifier.Mark(m),
	}
}

func (f ReferenceForm) Unexpand() common.Datum {
	return common.Pair{common.Symbol("#%reference"), common.Pair{f.Identifier.Datum(), common.Null}}
}

var patternLambda = common.MustCompileSimplePattern(read.MustReadDatum("(#%lambda (formals ...) body)"))

func transformLambda(c common.Continuation, args ...common.Datum) (common.Evaluation, error) {
	result, ok := patternLambda.Match(common.NewSyntax(args[0]))
	if !ok {
		return common.ErrorC(fmt.Errorf("#%lambda: bad syntax"))
	}
	var formals []common.Identifier
	for _, syntax := range result[common.Symbol("formals")].([]interface{}) {
		id, ok := syntax.(common.Syntax).Identifier()
		if !ok {
			return common.ErrorC(fmt.Errorf("#%lambda: bad syntax"))
		}
		formals = append(formals, id)
	}
	if common.DuplicateIdentifiers(formals...) {
		return common.ErrorC(fmt.Errorf("lambda: duplicate formals"))
	}
	var forms []common.Syntax
	for _, syntax := range result[common.Symbol("body")].([]interface{}) {
		forms = append(forms, syntax.(common.Syntax))
	}
	return common.CallC(c, LambdaForm{formals, forms})
}
