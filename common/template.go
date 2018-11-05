package common

import (
	"fmt"
	"strings"
)

type SyntaxTemplate struct {
	Template Datum
}

func (s SyntaxTemplate) Debug() string {
	return fmt.Sprintf("#'%v", Write(s.Template))
}

type Subtemplate struct {
	Subtemplate Datum
	Nesting     int
}

func (s Subtemplate) Write() string {
	var ellipsis []string
	for i := 0; i < s.Nesting; i++ {
		ellipsis = append(ellipsis, "...")
	}
	return fmt.Sprintf("%v %v", s.Subtemplate, strings.Join(ellipsis, " "))
}

type PatternVariableReference struct {
	PatternVariable *PatternVariable
}

func (t PatternVariableReference) Write() string {
	return fmt.Sprintf("%p", t.PatternVariable)
}
