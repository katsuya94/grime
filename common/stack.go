package common

type Location *interface{}

func NewLocation() Location {
	value := interface{}(nil)
	return &value
}

type Stack struct {
	frames []*Frame
}

func (s Stack) Get(ref StackFrameReference) interface{} {
	return *s.frames[len(s.frames)-1-int(ref.StackContext)].bindings[ref.FrameIndex]
}

func (s Stack) Set(ref StackFrameReference, i interface{}) {
	*s.frames[len(s.frames)-1-int(ref.StackContext)].bindings[ref.FrameIndex] = i
}

func (s Stack) Push(frame *Frame) Stack {
	new := Stack{make([]*Frame, len(s.frames)+1)}
	copy(new.frames, s.frames)
	new.frames[len(new.frames)-1] = frame
	return new
}

func NewStack(frame *Frame) Stack {
	return Stack{[]*Frame{frame}}
}

type Frame struct {
	bindings []Location
}

type StackContext int

var CurrentStackContext StackContext = 0

type BindingStackContext struct {
	Binding      Binding
	StackContext StackContext
}

func (bsc BindingStackContext) Nil() bool {
	return bsc == NilBindingStackContext
}

var NilBindingStackContext = BindingStackContext{nil, -1}

type StackFrameReference struct {
	StackContext StackContext
	FrameIndex   int
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

func (ft FrameTemplate) Instantiate() *Frame {
	frame := &Frame{make([]Location, ft.size)}
	for i := 0; i < ft.size; i++ {
		frame.bindings[i] = NewLocation()
	}
	return frame
}

func (ft FrameTemplate) Size() int {
	return ft.size
}
