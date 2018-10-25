package common

type identifier struct {
	name  Symbol
	marks int
}

func NewWrappedSyntax(d Datum) WrappedSyntax {
	return WrappedSyntax{make(map[identifier]Location), 0, d}
}

func (w WrappedSyntax) Datum() Datum {
	return w.datum
}

func (w WrappedSyntax) PushOnto(d Datum) WrappedSyntax {
	return WrappedSyntax{w.substitutions, w.marks, d}
}

func (w WrappedSyntax) Identifier() (Symbol, Location, bool) {
	name, ok := w.datum.(Symbol)
	if !ok {
		return Symbol(""), nil, false
	}
	location, _ := w.substitutions[identifier{name, w.marks}]
	return name, location, true
}

func (w WrappedSyntax) Set(name Symbol, location Location) WrappedSyntax {
	substitutions := make(map[identifier]Location, len(w.substitutions))
	for id, l := range w.substitutions {
		substitutions[id] = l
	}
	substitutions[identifier{name, w.marks}] = location
	return WrappedSyntax{substitutions, w.marks, w.datum}
}
