package r6rs_base

import (
	"fmt"

	"github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/lib/r6rs"
	"github.com/katsuya94/grime/read"
)

var syntaxTransformer = r6rs.NewCoreTransformer(transformSyntax)
var patternSyntax = common.MustCompileSimplePattern(read.MustReadDatum("(syntax template)"))

func transformSyntax(ctx r6rs.ExpansionContext, syntax common.Syntax, mark *common.M) (r6rs.CoreForm, error) {
	result, ok := patternSyntax.Match(syntax)
	if !ok {
		return nil, fmt.Errorf("syntax: bad syntax")
	}
	template := result[common.Symbol("template")].(common.Syntax)
	datum, unexpanded, err := expandTemplate(template, ctx.Env)
	if err != nil {
		return nil, err
	}
	bindings := make([]*common.Binding, 0, len(unexpanded))
	for binding, unexpanded := range unexpanded {
		if unexpanded > 0 {
			return nil, fmt.Errorf("in syntax template at %v: encountered unexpanded pattern variable", template.SourceLocation())
		}
		bindings = append(bindings, binding)
	}
	return syntaxForm{syntaxTemplateForm{datum, bindings}, mark}, nil
}

type syntaxTemplateForm struct {
	template common.Datum
	bindings []*common.Binding
}

type patternVariableUseForm struct {
	binding *common.Binding
}

type subTemplateForm struct {
	syntaxTemplateForm syntaxTemplateForm
	nesting            int
	expansionBindings  []*common.Binding
}

func expandTemplate(syntax common.Syntax, env common.Environment) (common.Datum, map[*common.Binding]int, error) {
	if id, ok := syntax.Identifier(); ok {
		if binding := id.Binding(); binding != nil {
			if role := env.Lookup(binding); role != nil {
				switch role := role.(type) {
				case common.PatternVariable:
					return patternVariableUseForm{id.Binding()}, map[*common.Binding]int{binding: role.Nesting}, nil
				case common.SyntacticAbstraction:
					if role.Transformer == common.EllipsisTransformer {
						return nil, nil, fmt.Errorf("compile: in syntax template at %v: improper use of ellipsis", id.SourceLocation())
					}
				}
			}
		}
		return syntax.Datum(), map[*common.Binding]int{}, nil
	}
	if pair, ok := syntax.Pair(); ok {
		first := common.NewSyntax(pair.First)
		rest := common.NewSyntax(pair.Rest)
		ellipsis := 0
		for {
			pair, ok := rest.Pair()
			if !ok {
				break
			}
			id, ok := common.NewSyntax(pair.First).Identifier()
			if !ok {
				break
			}
			binding := id.Binding()
			if binding == nil {
				break
			}
			role := env.Lookup(binding)
			if role == nil {
				break
			} else if role, ok := role.(common.SyntacticAbstraction); !ok {
				break
			} else if role.Transformer != common.EllipsisTransformer {
				break
			}
			ellipsis++
			rest = common.NewSyntax(pair.Rest)
		}
		firstCompiled, firstUnexpanded, err := expandTemplate(first, env)
		if err != nil {
			return nil, nil, err
		}
		firstStatic := len(firstUnexpanded) == 0
		if firstStatic && ellipsis > 0 {
			return nil, nil, fmt.Errorf("in syntax template at %v: syntax subtemplate must contain a pattern variable", first.SourceLocation())
		}
		restCompiled, restUnexpanded, err := expandTemplate(rest, env)
		if err != nil {
			return nil, nil, err
		}
		restStatic := len(restUnexpanded) == 0
		if firstStatic && restStatic {
			return syntax.Datum(), map[*common.Binding]int{}, nil
		}
		if ellipsis > 0 {
			expansionBindings := make([]*common.Binding, 0, len(firstUnexpanded))
			bindings := make([]*common.Binding, 0, len(firstUnexpanded))
			for binding, unexpanded := range firstUnexpanded {
				if unexpanded >= ellipsis {
					firstUnexpanded[binding] = unexpanded - ellipsis
					expansionBindings = append(expansionBindings, binding)
				}
				bindings = append(bindings, binding)
			}
			if len(expansionBindings) == 0 {
				return nil, nil, fmt.Errorf("in syntax template at %v: syntax subtemplate must contain a pattern variable determining expansion count", first.SourceLocation())
			}
			firstCompiled = subTemplateForm{syntaxTemplateForm{firstCompiled, bindings}, ellipsis, expansionBindings}
		}
		pairUnexpanded := map[*common.Binding]int{}
		for binding, unexpanded := range firstUnexpanded {
			pairUnexpanded[binding] = unexpanded
		}
		for binding, restUnexpanded := range restUnexpanded {
			if firstUnexpanded, ok := pairUnexpanded[binding]; ok {
				if restUnexpanded != firstUnexpanded {
					return nil, nil, fmt.Errorf("in syntax template at %v: incompatible expansion counts in first and rest of pair", syntax.SourceLocation())
				}
			}
			pairUnexpanded[binding] = restUnexpanded
		}
		return common.Pair{firstCompiled, restCompiled}, pairUnexpanded, nil
	}
	return syntax.Datum(), map[*common.Binding]int{}, nil
}

type syntaxForm struct {
	template syntaxTemplateForm
	mark     *common.M
}

func (f syntaxForm) CpsTransform(ctx *r6rs.CpsTransformContext) (common.Expression, error) {
	datum, err := cpsTransformSyntax(ctx, f.template.template)
	if err != nil {
		return nil, err
	}
	indices := make([]int, len(f.template.bindings))
	for i, binding := range f.template.bindings {
		index, err := ctx.Index(binding.Identifier())
		if err != nil {
			return nil, err
		}
		indices[i] = index
	}
	return syntax{syntaxTemplate{datum, indices}}, nil
}

type syntaxTemplate struct {
	template common.Datum
	indices  []int
}

type patternVariableUse struct {
	index int
}

type subTemplate struct {
	syntaxTemplate   syntaxTemplate
	nesting          int
	expansionIndices []int
}

func cpsTransformSyntax(ctx *r6rs.CpsTransformContext, datum common.Datum) (common.Datum, error) {
	switch datum := datum.(type) {
	case common.WrappedSyntax:
		return datum, nil
	case patternVariableUseForm:
		index, err := ctx.Index(datum.binding.Identifier())
		if err != nil {
			return nil, err
		}
		return patternVariableUse{index}, nil
	case subTemplateForm:
		template, err := cpsTransformSyntax(ctx, datum.syntaxTemplateForm.template)
		if err != nil {
			return nil, err
		}
		indices := make([]int, len(datum.syntaxTemplateForm.bindings))
		for i, binding := range datum.syntaxTemplateForm.bindings {
			index, err := ctx.Index(binding.Identifier())
			if err != nil {
				return nil, err
			}
			indices[i] = index
		}
		expansionIndices := make([]int, len(datum.expansionBindings))
		for i, binding := range datum.expansionBindings {
			index, err := ctx.Index(binding.Identifier())
			if err != nil {
				return nil, err
			}
			expansionIndices[i] = index
		}
		return subTemplate{syntaxTemplate{template, indices}, datum.nesting, expansionIndices}, nil
	case common.Pair:
		firstDatum, err := cpsTransformSyntax(ctx, datum.First)
		if err != nil {
			return nil, err
		}
		restDatum, err := cpsTransformSyntax(ctx, datum.Rest)
		if err != nil {
			return nil, err
		}
		return common.Pair{firstDatum, restDatum}, nil
	default:
		return nil, fmt.Errorf("cps transform: unhandled syntax template %v", common.Write(datum))
	}
}

type syntax struct {
	template syntaxTemplate
}

func (e syntax) Evaluate(ctx common.EvaluationContext, c common.Continuation) (common.Evaluation, error) {
	matches := make(map[int]interface{}, len(e.template.indices))
	for _, index := range e.template.indices {
		matches[index] = ctx.Get(index).Get()
	}
	datum, err := evaluateSyntaxTemplate(e.template.template, matches)
	if err != nil {
		return common.ErrorC(err)
	}
	return common.CallC(c, datum)
}

func evaluateSyntaxTemplate(datum common.Datum, matches map[int]interface{}) (common.Datum, error) {
	switch datum := datum.(type) {
	case common.WrappedSyntax:
		return datum, nil
	case patternVariableUse:
		return matches[datum.index].(common.Syntax).Datum(), nil
	case common.Pair:
		if first, ok := datum.First.(subTemplate); ok {
			data, err := evaluateSubtemplate(first, matches)
			if err != nil {
				return nil, err
			}
			rest, err := evaluateSyntaxTemplate(datum.Rest, matches)
			if err != nil {
				return nil, err
			}
			result := rest
			for i := len(data) - 1; i >= 0; i-- {
				result = common.Pair{data[i], result}
			}
			return result, nil
		}
		first, err := evaluateSyntaxTemplate(datum.First, matches)
		if err != nil {
			return nil, err
		}
		rest, err := evaluateSyntaxTemplate(datum.Rest, matches)
		if err != nil {
			return nil, err
		}
		return common.Pair{first, rest}, nil
	default:
		return nil, fmt.Errorf("unhandled syntax template %v", common.Write(datum))
	}
}

func evaluateSubtemplate(st subTemplate, matches map[int]interface{}) ([]common.Datum, error) {
	if st.nesting == 0 {
		datum, err := evaluateSyntaxTemplate(st.syntaxTemplate.template, matches)
		if err != nil {
			return nil, err
		}
		return []common.Datum{datum}, nil
	}
	n := len(matches[st.expansionIndices[0]].([]interface{}))
	for _, index := range st.expansionIndices {
		if len(matches[index].([]interface{})) != n {
			return nil, fmt.Errorf("differing number of matches for syntax template")
		}
	}
	var data []common.Datum
	for j := 0; j < n; j++ {
		nestedMatches := make(map[int]interface{}, len(st.syntaxTemplate.indices))
		for _, index := range st.syntaxTemplate.indices {
			nestedMatches[index] = matches[index]
		}
		for _, index := range st.expansionIndices {
			nestedMatches[index] = nestedMatches[index].([]interface{})[j]
		}
		nestedData, err := evaluateSubtemplate(subTemplate{st.syntaxTemplate, st.nesting - 1, st.expansionIndices}, nestedMatches)
		if err != nil {
			return nil, err
		}
		data = append(data, nestedData...)
	}
	return data, nil
}
