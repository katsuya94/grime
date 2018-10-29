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

type TemplatePair struct {
	First    Datum
	Ellipsis int
	Rest     Datum
}

func (t TemplatePair) Write() string {
	var ellipsis []string
	for i := 0; i < t.Ellipsis; i++ {
		ellipsis = append(ellipsis, "...")
	}
	return fmt.Sprintf("(%v %v . %v)", Write(t.First), strings.Join(ellipsis, " "), Write(t.Rest))
}

type PatternVariableReference struct {
	PatternVariable *PatternVariable
}

func (t PatternVariableReference) Write() string {
	return fmt.Sprintf("%p", t.PatternVariable)
}
