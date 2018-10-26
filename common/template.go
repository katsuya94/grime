package common

type Template interface{}

type TemplatePair struct {
	First Template
	Rest  Template
}

type PatternVariableReference struct {
	PatternVariable *PatternVariable
}

type Subtemplate struct {
	Template Template
	Nesting  int
}
