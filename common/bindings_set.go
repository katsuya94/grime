package common

// TODO: rename to LocationSet, also rename file.
type BindingSet map[int]map[Symbol]Location

func NewBindingSet() BindingSet {
	return BindingSet{}
}

func (set BindingSet) Get(id Symbol, phase int) Location {
	if _, ok := set[phase]; !ok {
		return nil
	}
	l, _ := set[phase][id]
	return l
}

func (set BindingSet) Set(id Symbol, phase int, location Location) {
	if _, ok := set[phase]; !ok {
		set[phase] = map[Symbol]Location{}
	}
	set[phase][id] = location
}

func (set BindingSet) Merge(other BindingSet) {
	for phase, locations := range other {
		if _, ok := set[phase]; !ok {
			set[phase] = map[Symbol]Location{}
		}
		for name, location := range locations {
			set[phase][name] = location
		}
	}
}
