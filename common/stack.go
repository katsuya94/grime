package common

type Stack struct {
	frames []Frame
}

func (s Stack) Get(ref StackFrameReference) interface{} {
	return *s.frames[len(s.frames)-1-ref.Stack].locations[ref.Frame]
}

func (s Stack) Set(ref StackFrameReference, i interface{}) {
	*s.frames[len(s.frames)-1-ref.Stack].locations[ref.Frame] = i
}

func NewStack(frame Frame) Stack {
	return Stack{[]Frame{frame}}
}

type Frame struct {
	locations []*interface{}
}

type StackFrameReference struct {
	Stack int
	Frame int
}

type FrameTemplate struct {
	size int
}

func NewFrameTemplate() FrameTemplate {
	return FrameTemplate{0}
}

func (ft *FrameTemplate) Add() int {
	i := ft.size
	ft.size++
	return i
}

func (ft FrameTemplate) Instantiate() Frame {
	frame := Frame{make([]*interface{}, ft.size)}
	for i := 0; i < ft.size; i++ {
		location := interface{}(nil)
		frame.locations[i] = &location
	}
	return frame
}
