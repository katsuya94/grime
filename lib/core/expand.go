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

func Expand(compiler Compiler, form common.Syntax) (common.Syntax, bool, error) {
	// TODO: what happens if we limit this to WrappedSyntax only?
	// r6rs-lib 12.3 seems to imply that transformers should only take wrapped syntax objects,
	// however macro uses would then often be unable to expand into macro uses
	if form, ok, err := expandMacroMatching(form, PatternMacroUseSet); ok || err != nil {
		return form, ok, err
	}
	if form, ok, err := expandMacroMatching(form, PatternMacroUseList); ok || err != nil {
		return form, ok, err
	}
	if form, ok, err := expandMacroMatching(form, PatternMacroUseImproperList); ok || err != nil {
		return form, ok, err
	}
	if form, ok, err := expandMacroMatching(form, PatternMacroUseSingletonIdentifier); ok || err != nil {
		return form, ok, err
	}
	if result, ok := PatternApplication.Match(form); ok {
		procedure := result[common.Symbol("procedure")].(common.Syntax)
		var arguments []common.Syntax
		for _, syntax := range result[common.Symbol("arguments")].([]interface{}) {
			arguments = append(arguments, syntax.(common.Syntax))
		}
		return common.NewSyntax(ApplicationForm{procedure, arguments}), true, nil
	}
	id, ok := form.Identifier()
	if ok {
		return common.NewSyntax(ReferenceForm{id}), true, nil
	}
	return common.Syntax{}, false, nil
}

func expandMacroMatching(syntax common.Syntax, pattern common.SimplePattern) (common.Syntax, bool, error) {
	result, ok := pattern.Match(syntax)
	if !ok {
		return common.Syntax{}, false, nil
	}
	id, ok := result[common.Symbol("keyword")].(common.Syntax).Identifier()
	if !ok {
		return common.Syntax{}, false, nil
	}
	location := id.Location()
	if location == nil {
		return common.Syntax{}, false, nil
	}
	keyword, ok := location.(*common.Keyword)
	if !ok {
		return common.Syntax{}, false, nil
	}
	if keyword.Transformer == nil {
		return common.Syntax{}, false, fmt.Errorf("expand: cannot use keyword before definition")
	}
	mark := common.NewMark()
	input := syntax.Mark(mark)
	output, err := util.Invoke(keyword.Transformer, input.Datum())
	if err != nil {
		return common.Syntax{}, false, err
	}
	return common.NewSyntax(output).Mark(mark), true, nil
}
