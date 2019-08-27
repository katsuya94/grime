package common

import "fmt"

type SyntaxTemplate struct {
	template Datum
	Bindings []*Binding
}

type patternVariableUse struct {
	binding *Binding
}

type subTemplate struct {
	syntaxTemplate    SyntaxTemplate
	nesting           int
	expansionBindings []*Binding
}

func CompileSyntaxTemplate(syntax Syntax, env Environment) (SyntaxTemplate, error) {
	datum, unexpanded, err := compileSyntaxTemplate(syntax, env)
	if err != nil {
		return SyntaxTemplate{}, err
	}
	bindings := make([]*Binding, 0, len(unexpanded))
	for binding, unexpanded := range unexpanded {
		if unexpanded > 0 {
			return SyntaxTemplate{}, fmt.Errorf("in syntax template at %v: encountered unexpanded pattern variable", syntax.SourceLocation())
		}
		bindings = append(bindings, binding)
	}
	return SyntaxTemplate{datum, bindings}, nil
}

func compileSyntaxTemplate(syntax Syntax, env Environment) (Datum, map[*Binding]int, error) {
	if id, ok := syntax.Identifier(); ok {
		if binding := id.Binding(); binding != nil {
			if role := env.Lookup(binding); role != nil {
				switch role := role.(type) {
				case PatternVariable:
					return patternVariableUse{id.Binding()}, map[*Binding]int{binding: role.nesting}, nil
				case SyntacticAbstraction:
					if role.Transformer == EllipsisTransformer {
						return nil, nil, fmt.Errorf("in syntax template at %v: improper use of ellipsis", id.SourceLocation())
					}
				}
			}
		}
		return syntax.Datum(), map[*Binding]int{}, nil
	}
	if pair, ok := syntax.Pair(); ok {
		first := NewSyntax(pair.First)
		rest := NewSyntax(pair.Rest)
		ellipsis := 0
		for {
			pair, ok := rest.Pair()
			if !ok {
				break
			}
			id, ok := NewSyntax(pair.First).Identifier()
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
			} else if role, ok := role.(SyntacticAbstraction); !ok {
				break
			} else if role.Transformer != EllipsisTransformer {
				break
			}
			ellipsis++
			rest = NewSyntax(pair.Rest)
		}
		firstCompiled, firstUnexpanded, err := compileSyntaxTemplate(first, env)
		if err != nil {
			return nil, nil, err
		}
		firstStatic := len(firstUnexpanded) == 0
		if firstStatic && ellipsis > 0 {
			return nil, nil, fmt.Errorf("in syntax template at %v: syntax subtemplate must contain a pattern variable", first.SourceLocation())
		}
		restCompiled, restUnexpanded, err := compileSyntaxTemplate(rest, env)
		if err != nil {
			return nil, nil, err
		}
		restStatic := len(restUnexpanded) == 0
		if firstStatic && restStatic {
			return syntax.Datum(), map[*Binding]int{}, nil
		}
		if ellipsis > 0 {
			expansionBindings := make([]*Binding, 0, len(firstUnexpanded))
			bindings := make([]*Binding, 0, len(firstUnexpanded))
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
			firstCompiled = subTemplate{SyntaxTemplate{firstCompiled, bindings}, ellipsis, expansionBindings}
		}
		pairUnexpanded := map[*Binding]int{}
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
		return Pair{firstCompiled, restCompiled}, pairUnexpanded, nil
	}
	return syntax.Datum(), map[*Binding]int{}, nil
}

func (st SyntaxTemplate) Evaluate(matches map[*Binding]interface{}) (Datum, error) {
	return evaluateSyntaxTemplate(st.template, matches)
}

func evaluateSyntaxTemplate(template Datum, matches map[*Binding]interface{}) (Datum, error) {
	switch template := template.(type) {
	case WrappedSyntax:
		return template, nil
	case patternVariableUse:
		return matches[template.binding].(Syntax).Datum(), nil
	case Pair:
		if first, ok := template.First.(subTemplate); ok {
			data, err := evaluateSubtemplate(first, matches)
			if err != nil {
				return nil, err
			}
			rest, err := evaluateSyntaxTemplate(template.Rest, matches)
			if err != nil {
				return nil, err
			}
			result := rest
			for i := len(data) - 1; i >= 0; i-- {
				result = Pair{data[i], result}
			}
			return result, nil
		}
		first, err := evaluateSyntaxTemplate(template.First, matches)
		if err != nil {
			return nil, err
		}
		rest, err := evaluateSyntaxTemplate(template.Rest, matches)
		if err != nil {
			return nil, err
		}
		return Pair{first, rest}, nil
	default:
		return nil, fmt.Errorf("unhandled syntax template %v", Write(template))
	}
}

func evaluateSubtemplate(st subTemplate, matches map[*Binding]interface{}) ([]Datum, error) {
	if st.nesting == 0 {
		datum, err := evaluateSyntaxTemplate(st.syntaxTemplate.template, matches)
		if err != nil {
			return nil, err
		}
		return []Datum{datum}, nil
	}
	n := len(matches[st.expansionBindings[0]].([]interface{}))
	for _, binding := range st.expansionBindings {
		if len(matches[binding].([]interface{})) != n {
			return nil, fmt.Errorf("differing number of matches for syntax template")
		}
	}
	var data []Datum
	for j := 0; j < n; j++ {
		nestedMatches := make(map[*Binding]interface{}, len(st.syntaxTemplate.Bindings))
		for _, binding := range st.syntaxTemplate.Bindings {
			nestedMatches[binding] = matches[binding]
		}
		for _, binding := range st.expansionBindings {
			nestedMatches[binding] = nestedMatches[binding].([]interface{})[j]
		}
		nestedData, err := evaluateSubtemplate(subTemplate{st.syntaxTemplate, st.nesting - 1, st.expansionBindings}, nestedMatches)
		if err != nil {
			return nil, err
		}
		data = append(data, nestedData...)
	}
	return data, nil
}
