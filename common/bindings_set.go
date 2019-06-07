package common

// TODO: make bindingSet private, rename file
type BindingsFrame struct {
	bindingSet BindingSet
	frame      *Frame
}

var EmptyBindingsFrame = BindingsFrame{}

func NewBindingsFrame() BindingsFrame {
	return BindingsFrame{NewBindingSet(), NewFrame()}
}

func (bfs BindingsFrame) Empty() bool {
	return bfs.bindingSet == nil && bfs.frame == nil
}

// TODO: this seems invasive
func (bfs BindingsFrame) Get(id Symbol, phase int) Binding {
	return bfs.bindingSet.Get(id, phase)
}

func (bfs BindingsFrame) Copy(id Symbol, phase int, binding Binding, frame *Frame) {
	binding = binding.Copy(frame, bfs.frame.NewBuilder())
	bfs.bindingSet.Set(id, phase, binding)
}

func (bfs BindingsFrame) CopyFrom(id Symbol, phase int, other BindingsFrame) {
	binding := other.bindingSet.Get(id, phase)
	bfs.Copy(id, phase, binding, other.frame)
}

type IdentifierTransformerFactory interface {
	New() IdentifierTransformer
}

type IdentifierTransformer interface {
	Transform(Symbol) (Symbol, bool)
	Error() error
}

type identifierTransformerFactoryAll struct{}

func (identifierTransformerFactoryAll) New() IdentifierTransformer {
	return identifierTransformerAll{}
}

type identifierTransformerAll struct{}

func (identifierTransformerAll) Transform(name Symbol) (Symbol, bool) {
	return name, true
}

func (identifierTransformerAll) Error() error {
	return nil
}

var IdentifierTransformerFactoryAll = identifierTransformerFactoryAll{}

type BindingSet map[int]map[Symbol]Binding

func NewBindingSet() BindingSet {
	return BindingSet{}
}

func (set BindingSet) Get(id Symbol, phase int) Binding {
	if _, ok := set[phase]; !ok {
		return nil
	}
	return set[phase][id]
}

func (set BindingSet) Set(id Symbol, phase int, binding Binding) {
	if _, ok := set[phase]; !ok {
		set[phase] = map[Symbol]Binding{}
	}
	set[phase][id] = binding
}
