package common

// TODO: *interface{} should be Location
// TODO: rename Location
type Location interface {
	Export() (*interface{}, bool)
}

// Keyword binds a syntax transformer in the region where the location is bound.
type Keyword struct {
	Transformer Procedure
}

func (l Keyword) Export() (*interface{}, bool) {
	return nil, false
}

// Variable binds a value in the region where the location is bound.
type Variable struct {
	Value Datum
}

func (l Variable) Export() (*interface{}, bool) {
	return nil, false
}

// PatternVariable binds a match result in the region where the location is bound.
type PatternVariable struct {
	Match   interface{}
	Nesting int
}

func (l PatternVariable) Export() (*interface{}, bool) {
	return nil, false
}

// Literal binds a pattern literal in the pattern for which the literal was given.
type Literal struct {
	Id Identifier
}

func (l Literal) Export() (*interface{}, bool) {
	return nil, false
}
