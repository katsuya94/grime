package common

type Location interface{}

type Keyword struct {
	Transformer Procedure
}
type Variable struct {
	Value Datum
}

type PatternVariable struct {
	Match   interface{}
	Nesting int
}

type BindingSet map[int]map[Symbol]Location

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
