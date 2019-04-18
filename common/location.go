package common

type Location interface{}

// Keyword binds a syntax transformer in the region where the location is bound.
type Keyword struct {
	Transformer Procedure
}

// Variable binds a value in the region where the location is bound.
type Variable struct {
	Value Datum
}

// PatternVariable binds a match result in the region where the location is bound.
type PatternVariable struct {
	Match   interface{}
	Nesting int
}

// Literal binds a pattern literal in the pattern for which the literal was given.
type Literal struct {
	Id Identifier
}
