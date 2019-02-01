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
