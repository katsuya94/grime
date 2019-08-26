package common

import "fmt"

type underscoreTransformer struct{}

func (underscoreTransformer) Call(c Continuation, args ...Datum) (Evaluation, error) {
	return ErrorC(fmt.Errorf("_: bad syntax"))
}

type ellipsisTransformer struct{}

func (ellipsisTransformer) Call(c Continuation, args ...Datum) (Evaluation, error) {
	return ErrorC(fmt.Errorf("...: bad syntax"))
}

var (
	UnderscoreTransformer = underscoreTransformer{}
	EllipsisTransformer   = ellipsisTransformer{}
)

type Pattern interface {
	Match(Syntax) (map[*Binding]interface{}, bool)
}

type patternLiteral struct {
	id Identifier
}

func (p patternLiteral) Match(syntax Syntax) (map[*Binding]interface{}, bool) {
	id, ok := syntax.Identifier()
	if !ok {
		return nil, false
	}
	if !id.FreeEqual(p.id) {
		return nil, false
	}
	return map[*Binding]interface{}{}, true
}

type patternUnderscore struct{}

func (p patternUnderscore) Match(syntax Syntax) (map[*Binding]interface{}, bool) {
	return map[*Binding]interface{}{}, true
}

type patternPatternVariable struct {
	patternVariable *Binding
}

func (p patternPatternVariable) Match(syntax Syntax) (map[*Binding]interface{}, bool) {
	return map[*Binding]interface{}{p.patternVariable: syntax}, true
}

type patternEllipsis struct {
	subPattern          Pattern
	subPatternVariables []*Binding
	restPattern         Pattern
}

func (p patternEllipsis) Match(syntax Syntax) (map[*Binding]interface{}, bool) {
	var (
		result     map[*Binding]interface{}
		subResults []map[*Binding]interface{}
	)
	for {
		if pair, ok := syntax.Pair(); ok {
			// If the syntax is a pair...
			first := NewSyntax(pair.First)
			rest := NewSyntax(pair.Rest)
			if subResult, ok := p.subPattern.Match(first); ok {
				// And its first matches subPattern...
				subResults = append(subResults, subResult)
				if restResult, ok := p.restPattern.Match(rest); ok {
					// And its rest matches restPattern...
					if _, ok := p.Match(rest); !ok {
						// But its rest does not match the ellipsis, we are done.
						result = restResult
						break
					}
				}
				// And the rest doesn't need to match restPattern, continue.
				syntax = rest
				continue
			}
		}
		// If the syntax is not a pair or its first doesn't match subPattern...
		if restResult, ok := p.restPattern.Match(syntax); ok {
			// And the syntax matches restPattern, we are done.
			result = restResult
			break
		}
		// And the syntax does not match restPattern, fail.
		return nil, false
	}
	for _, patternVariable := range p.subPatternVariables {
		result[patternVariable] = []interface{}{}
	}
	for _, subResult := range subResults {
		for _, patternVariable := range p.subPatternVariables {
			result[patternVariable] = append(result[patternVariable].([]interface{}), subResult[patternVariable])
		}
	}
	return result, true
}

type patternPair struct {
	first Pattern
	rest  Pattern
}

func (p patternPair) Match(syntax Syntax) (map[*Binding]interface{}, bool) {
	pair, ok := syntax.Pair()
	if !ok {
		return nil, false
	}
	firstResult, ok := p.first.Match(NewSyntax(pair.First))
	if !ok {
		return nil, false
	}
	restResult, ok := p.rest.Match(NewSyntax(pair.Rest))
	if !ok {
		return nil, false
	}
	result := map[*Binding]interface{}{}
	for patternVariable, match := range firstResult {
		result[patternVariable] = match
	}
	for patternVariable, match := range restResult {
		result[patternVariable] = match
	}
	return result, true
}

type patternDatum struct {
	datum Datum
}

func (p patternDatum) Match(syntax Syntax) (map[*Binding]interface{}, bool) {
	if syntax.Unwrap() != p.datum {
		return nil, false
	}
	return map[*Binding]interface{}{}, true
}

type PatternVariableInfo struct {
	Binding *Binding
	Nesting int
}

func CompilePattern(syntax Syntax, scope *Scope, phase int, env Environment) (Pattern, []PatternVariableInfo, error) {
	if syntax, ok := syntax.Identifier(); ok {
		role := syntax.Role(env)
		if role != nil {
			if role, ok := role.(PatternLiteral); ok {
				return patternLiteral{role.Id}, nil, nil
			}
			if role, ok := role.(SyntacticAbstraction); ok {
				if role.Transformer == UnderscoreTransformer {
					return patternUnderscore{}, nil, nil
				}
				if role.Transformer == EllipsisTransformer {
					return nil, nil, fmt.Errorf("pattern: invalid use of ellipsis")
				}
			}
		}
		_, patternVariableBinding := Bind(syntax, scope, phase)
		return patternPatternVariable{patternVariableBinding}, []PatternVariableInfo{{patternVariableBinding, 0}}, nil
	}
	if syntax, ok := syntax.Pair(); ok {
		if cdr, ok := NewSyntax(syntax.Rest).Pair(); ok {
			if cadr, ok := NewSyntax(cdr.First).Identifier(); ok {
				role := cadr.Role(env)
				if role != nil {
					if role, ok := role.(SyntacticAbstraction); ok {
						if role.Transformer == EllipsisTransformer {
							subPattern, subPatternVariableInfos, err := CompilePattern(NewSyntax(syntax.First), scope, phase, env)
							if err != nil {
								return nil, nil, err
							}
							subPatternVariables := make([]*Binding, len(subPatternVariableInfos))
							for i := range subPatternVariableInfos {
								subPatternVariableInfos[i].Nesting++
								subPatternVariables[i] = subPatternVariableInfos[i].Binding
							}
							cddr := NewSyntax(cdr.Rest)
							restPattern, restPatternVariableInfos, err := CompilePattern(cddr, scope, phase, env)
							if err != nil {
								return nil, nil, err
							}
							patternVariableInfos := append(subPatternVariableInfos, restPatternVariableInfos...)
							return patternEllipsis{subPattern, subPatternVariables, restPattern}, patternVariableInfos, nil
						}
					}
				}
			}
		}
		firstPattern, firstPatternVariableInfos, err := CompilePattern(NewSyntax(syntax.First), scope, phase, env)
		if err != nil {
			return nil, nil, err
		}
		restPattern, restPatternVariableInfos, err := CompilePattern(NewSyntax(syntax.Rest), scope, phase, env)
		if err != nil {
			return nil, nil, err
		}
		patternVariableInfos := append(firstPatternVariableInfos, restPatternVariableInfos...)
		return patternPair{firstPattern, restPattern}, patternVariableInfos, nil
	}
	return patternDatum{syntax.Unwrap()}, nil, nil
}
