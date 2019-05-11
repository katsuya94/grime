package common

// TODO: rename to LocationSet, also rename file.
type BindingSet map[int]map[Symbol]Binding

func NewBindingSet() BindingSet {
	return BindingSet{}
}

func (set BindingSet) Get(id Symbol, phase int) Binding {
	if _, ok := set[phase]; !ok {
		return nil
	}
	l, _ := set[phase][id]
	return l
}

func (set BindingSet) Set(id Symbol, phase int, binding Binding) {
	if _, ok := set[phase]; !ok {
		set[phase] = map[Symbol]Binding{}
	}
	set[phase][id] = binding
}

func (set BindingSet) Merge(other BindingSet) {
	for phase, bindings := range other {
		if _, ok := set[phase]; !ok {
			set[phase] = map[Symbol]Binding{}
		}
		for name, binding := range bindings {
			set[phase][name] = binding
		}
	}
}
