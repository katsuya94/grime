package read

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
