package common

// TODO: bindings no longer have to be pointers. One issue with this is special bingings like _ and ...
// TODO: make indices private

type Binding interface {
	Copy(src *Frame, dest FrameBuilder) Binding
}

// Keyword binds a syntax transformer in the region where bound.
type Keyword struct {
	Transformer           Procedure
	TransformerFrameIndex int
}

func NewKeyword(frameTemplate *FrameTemplate) *Keyword {
	return &Keyword{nil, frameTemplate.Add()}
}

func (l *Keyword) Copy(src *Frame, dest FrameBuilder) Binding {
	// TODO: temporary hack to get shared keywords to work
	if l == UnderscoreKeyword {
		return UnderscoreKeyword
	}
	if l == EllipsisKeyword {
		return EllipsisKeyword
	}
	return &Keyword{l.Transformer, dest.Copy(src, l.TransformerFrameIndex)}
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

func (l Variable) Copy(src *Frame, dest FrameBuilder) Binding {
	return &Variable{l.Value, dest.Copy(src, l.ValueFrameIndex)}
}

func (l Variable) ValueReference(stackContext StackContext) StackFrameReference {
	return StackFrameReference{stackContext, l.ValueFrameIndex}
}

// PatternVariable binds a match result in the region where bound.
type PatternVariable struct {
	Match           interface{}
	MatchFrameIndex int
	Nesting         int
}

func NewPatternVariable(frameTemplate *FrameTemplate) *PatternVariable {
	return &PatternVariable{nil, frameTemplate.Add(), 0}
}

func (l PatternVariable) Copy(src *Frame, dest FrameBuilder) Binding {
	return &PatternVariable{l.Match, dest.Copy(src, l.MatchFrameIndex), l.Nesting}
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

func (l Literal) Copy(src *Frame, dest FrameBuilder) Binding {
	return l
}
