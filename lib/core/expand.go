package core

import (
	"fmt"

	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/read"
	"github.com/katsuya94/grime/util"
)

var (
	PatternMacroUseList                = common.Pattern(read.MustReadDatum("(keyword _ ...)"))
	PatternMacroUseImproperList        = common.Pattern(read.MustReadDatum("(keyword _ ... . _)"))
	PatternMacroUseSingletonIdentifier = common.Pattern(read.MustReadDatum("keyword"))
	PatternMacroUseSet                 = common.Pattern(read.MustReadDatum("(set! keyword _)"))
	PatternApplication                 = common.Pattern(read.MustReadDatum("(procedure arguments ...)"))
)

var setIdentifier = common.NewIdentifier(common.Symbol("set!")).Bind(setKeyword)

func Expand(compiler Compiler, form common.Datum) (common.Datum, bool, error) {
	// TODO: what happens if we limit this to WrappedSyntax only?
	// r6rs-lib 12.3 seems to imply that transformers should only take wrapped syntax objects,
	// however macro uses would then often be unable to expand into macro uses
	if form, ok, err := expandMacroMatching(form, PatternMacroUseSet, []common.Identifier{setIdentifier}); ok || err != nil {
		return form, ok, err
	}
	if form, ok, err := expandMacroMatching(form, PatternMacroUseList, nil); ok || err != nil {
		return form, ok, err
	}
	if form, ok, err := expandMacroMatching(form, PatternMacroUseImproperList, nil); ok || err != nil {
		return form, ok, err
	}
	if form, ok, err := expandMacroMatching(form, PatternMacroUseSingletonIdentifier, nil); ok || err != nil {
		return form, ok, err
	}
	if result, ok, err := common.MatchSyntaxSimple(common.NewSyntax(form), PatternApplication); err != nil {
		return nil, false, err
	} else if ok {
		procedure := result.Get("procedure").(common.Syntax).Datum()
		var arguments []common.Datum
		for _, syntax := range result.Get("arguments").([]interface{}) {
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

func expandMacroMatching(form common.Datum, pattern common.Syntax, literals []common.Identifier) (common.Datum, bool, error) {
	result, ok, err := common.MatchSyntax(common.NewSyntax(form), pattern, literals)
	if err != nil {
		return nil, false, err
	} else if !ok {
		return nil, false, nil
	}
	id, ok := result.Get(common.NewIdentifier(common.Symbol("keyword"))).(common.Syntax).Identifier()
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
	input := common.Mark(form, mark)
	output, err := util.Invoke(keyword.Transformer, input)
	if err != nil {
		return nil, false, err
	}
	return common.Mark(output, mark), true, nil
}
