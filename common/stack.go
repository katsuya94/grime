package common

type Stack []Frame

func (s Stack) Get(ref StackFrameReference) interface{} {
	return s[len(s)-1-ref.Stack][ref.Frame]
}

func (s Stack) Set(ref StackFrameReference, i interface{}) {
	s[len(s)-1-ref.Stack][ref.Frame] = i
}

func NewStack() Stack {
	return nil
}

type Frame []interface{}

type StackFrameReference struct {
	Stack int
	Frame int
}
