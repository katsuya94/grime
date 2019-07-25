package r6rs

import (
	"fmt"

	"github.com/katsuya94/grime/common"
)

var GlobalBindingsTable = NewBindingsTable()

type Binding int

var nextBinding = Binding(0)

func (b Binding) String() string {
	return fmt.Sprintf("%#8x", b)
}

type bindingsTableEntry struct {
	id      common.Identifier
	binding Binding
}

type BindingsTable map[common.Symbol][]bindingsTableEntry

func NewBindingsTable() BindingsTable {
	return BindingsTable{}
}

func (bt BindingsTable) Bind(id common.Identifier) Binding {
	name := id.Name()
	binding := nextBinding
	nextBinding++
	bt[name] = append(bt[name], bindingsTableEntry{id, binding})
	return binding
}

func (bt BindingsTable) Lookup(id common.Identifier) (Binding, bool) {
	return Binding(-1), false
}
