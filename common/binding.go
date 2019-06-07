package common

// TODO: bindings no longer have to be pointers. One issue with this is special bingings like _ and ...
// TODO: make indices private, instead expose methods that return values given a frame

type Binding interface {
	Copy(src *Frame, dest FrameBuilder) Binding
}

type KeywordFactory struct {
	Transformer Procedure
}

func (bf KeywordFactory) New(frameBuilder FrameBuilder) Binding {
	return &Keyword{bf.Transformer, frameBuilder.Add(bf.Transformer)}
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

type VariableFactory struct {
	Value Datum
}

func (bf VariableFactory) New(frameBuilder FrameBuilder) Binding {
	return &Variable{bf.Value, frameBuilder.Add(bf.Value)}
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

type PatternVariableFactory struct {
	Match   interface{}
	Nesting int
}

func (bf PatternVariableFactory) New(frameBuilder FrameBuilder) Binding {
	return &PatternVariable{bf.Match, frameBuilder.Add(bf.Match), bf.Nesting}
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

type LiteralFactory struct {
	Id Identifier
}

func (bf LiteralFactory) New(frameBuilder FrameBuilder) Binding {
	return &Literal{bf.Id}
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
