package common

type SourceLocation struct {
	File   string
	Line   int
	Column int
	Offset int
	Length int
}

type SourceLocationTree struct {
	SourceLocation
	Children Datum
}
