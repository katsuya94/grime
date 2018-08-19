package core

type Datum interface{}

type Boolean bool
type Number string
type Character rune
type String string
type Symbol string
type Pair struct {
	First Datum
	Rest  Datum
}
type WrappedSyntax struct {
	Form Datum
	// TODO add marks and substitutions
}
type Procedure func(*Environment, ...Datum) (Datum, error)
type Application struct {
	Procedure Datum
	Arguments []Datum
}
type Quote struct {
	Datum Datum
}
type If struct {
	Condition Datum
	Then      Datum
	Else      Datum
}
type SyntaxDefinition struct {
	Name Symbol
	Form Datum
}
type Definition struct {
	Name Symbol
	Form Datum
}
type Begin struct {
	Forms []Datum
}
type LetSyntax struct{}
