package common

// TODO: *interface{} should be Binding
// TODO: rename Binding
type Binding interface {
	Export() (*interface{}, bool)
}

// Keyword binds a syntax transformer in the region where bound.
type Keyword struct {
	Transformer           Procedure
	TransformerFrameIndex int
}

func NewKeyword(frameTemplate *FrameTemplate) *Keyword {
	return &Keyword{nil, frameTemplate.Add()}
}

func (l Keyword) Export() (*interface{}, bool) {
	return nil, false
}

func (l Keyword) TransformerReference(stackContext StackContext) StackFrameReference {
	return StackFrameReference{stackContext, l.TransformerFrameIndex}
}

// Variable binds a value in the region where bound.
type Variable struct {
	Value           Datum
	ValueFrameIndex int
}

func NewVariable(frameTemplate *FrameTemplate) *Variable {
	return &Variable{nil, frameTemplate.Add()}
}

func (l Variable) Export() (*interface{}, bool) {
	return nil, false
}

func (l Variable) ValueReference(stackContext StackContext) StackFrameReference {
	return StackFrameReference{stackContext, l.ValueFrameIndex}
}

// PatternVariable binds a match result in the region where bound.
type PatternVariable struct {
	Match           interface{}
	Nesting         int
	MatchFrameIndex int
}

func NewPatternVariable(frameTemplate *FrameTemplate) *PatternVariable {
	return &PatternVariable{nil, 0, frameTemplate.Add()}
}

func (l PatternVariable) Export() (*interface{}, bool) {
	return nil, false
}

func (l PatternVariable) PatternVariableReference(stackContext StackContext) StackFrameReference {
	return StackFrameReference{stackContext, l.MatchFrameIndex}
}

// Literal binds a pattern literal in the pattern where bound.
type Literal struct {
	Id Identifier
}

func NewLiteral() *Literal {
	return &Literal{}
}

func (l Literal) Export() (*interface{}, bool) {
	return nil, false
}
