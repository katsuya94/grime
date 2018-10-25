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

func (w WrappedSyntax) PushDown(d Datum) WrappedSyntax {
	return WrappedSyntax{w.substitutions, w.marks, d}
}
