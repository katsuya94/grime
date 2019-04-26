package core

import (
	"fmt"

	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/read"
	"github.com/katsuya94/grime/util"
)

var setIdentifier = common.NewIdentifier(common.Symbol("set!")).Bind(setKeyword)

var (
	PatternMacroUseList                = common.MustCompileSimplePattern(read.MustReadDatum("(keyword _ ...)"))
	PatternMacroUseImproperList        = common.MustCompileSimplePattern(read.MustReadDatum("(keyword _ ... . _)"))
	PatternMacroUseSingletonIdentifier = common.MustCompileSimplePattern(read.MustReadDatum("keyword"))
	PatternMacroUseSet                 = common.MustCompileSimplePatternWithIdentifierLiterals(read.MustReadDatum("(set! keyword _)"), setIdentifier)
	PatternApplication                 = common.MustCompileSimplePattern(read.MustReadDatum("(procedure arguments ...)"))
)

func Expand(compiler Compiler, form common.Datum) (common.Datum, bool, error) {
	// TODO: what happens if we limit this to WrappedSyntax only?
	// r6rs-lib 12.3 seems to imply that transformers should only take wrapped syntax objects,
	// however macro uses would then often be unable to expand into macro uses
	syntax := common.NewSyntax(form)
	if form, ok, err := expandMacroMatching(syntax, PatternMacroUseSet); ok || err != nil {
		return form, ok, err
	}
	if form, ok, err := expandMacroMatching(syntax, PatternMacroUseList); ok || err != nil {
		return form, ok, err
	}
	if form, ok, err := expandMacroMatching(syntax, PatternMacroUseImproperList); ok || err != nil {
		return form, ok, err
	}
	if form, ok, err := expandMacroMatching(syntax, PatternMacroUseSingletonIdentifier); ok || err != nil {
		return form, ok, err
	}
	if result, ok := PatternApplication.Match(common.NewSyntax(syntax)); ok {
		procedure := result[common.Symbol("procedure")].(common.Syntax).Datum()
		var arguments []common.Datum
		for _, syntax := range result[common.Symbol("arguments")].([]interface{}) {
			arguments = append(arguments, syntax.(common.Syntax).Datum())
		}
		return ApplicationForm{procedure, arguments}, true, nil
	}
	id, ok := common.NewSyntax(form).Identifier()
	if ok {
		return ReferenceForm{id}, true, nil
	}
	return nil, false, nil
}

func expandMacroMatching(syntax common.Syntax, pattern common.SimplePattern) (common.Datum, bool, error) {
	result, ok := pattern.Match(syntax)
	if !ok {
		return nil, false, nil
	}
	id, ok := result[common.Symbol("keyword")].(common.Syntax).Identifier()
	if !ok {
		return nil, false, nil
	}
	location := id.Location()
	if location == nil {
		return nil, false, nil
	}
	keyword, ok := location.(*common.Keyword)
	if !ok {
		return nil, false, nil
	}
	if keyword.Transformer == nil {
		return nil, false, fmt.Errorf("expand: cannot use keyword before definition")
	}
	mark := common.NewMark()
	input := common.Mark(syntax.Datum(), mark)
	output, err := util.Invoke(keyword.Transformer, input)
	if err != nil {
		return nil, false, err
	}
	return common.Mark(output, mark), true, nil
}
