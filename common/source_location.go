package common

import "fmt"

type SourceLocation struct {
	File   string
	Line   int
	Column int
	Offset int
	Length int
}

func (sourceLocation SourceLocation) String() string {
	if (sourceLocation == SourceLocation{}) {
		return "(unknown)"
	}
	return fmt.Sprintf("%v:%v:%v", sourceLocation.File, sourceLocation.Line+1, sourceLocation.Column+1)
}

type SourceLocationTree struct {
	SourceLocation
	Children Datum
}
