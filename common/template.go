package common

import "fmt"

type Template interface {
	Expression
}

type TemplatePair struct {
	First    Template
	Ellipsis int
	Rest     Template
}

func (TemplatePair t) Debug() string {

}

type PatternVariableReference struct {
	PatternVariable *PatternVariable
}

func (PatternVariableReference t) Debug() string {
	fmt.Printf("%p", t.PatternVariable)
}
