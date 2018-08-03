package grime

var symbols map[string]*Symbol = make(map[string]*Symbol)

func intern(name string) *Symbol {
	symbol, ok := symbols[name]
	if !ok {
		symbol = &Symbol{name}
		symbols[name] = symbol
	}
	return symbol
}

func equal(a AST, b AST) bool {
	if a == nil && b == nil {
		return true
	}
	if a == nil || b == nil {
		return false
	}
	return a.Equal(b)
}

type Pair struct {
	First AST
	Rest AST
}

func (pair *Pair) Equal(ast AST) bool {
	other, ok := ast.(*Pair)
	if !ok {
		return false
	}
	return equal(pair.First, other.First) && equal(pair.Rest, other.Rest)
}

type Symbol struct{
	name string
}

func (symbol *Symbol) Equal(ast AST) bool {
	other, ok := ast.(*Symbol)
	if !ok {
		return false
	}
	return symbol == other
}

type String string

func (string String) Equal(ast AST) bool {
	other, ok := ast.(String)
	if !ok {
		return false
	}
	return string == other
}

type AST interface {
	Equal(AST) bool
}
