package read

import (
	"fmt"

	"github.com/katsuya94/grime/common"
)

type ReadError struct {
	common.SourceLocation
	message string
}

func (e ReadError) Error() string {
	return fmt.Sprintf("read: at %v: %v", e.String(), e.message)
}

type UnexpectedEOFError struct {
	ReadError
}

func NewUnexpectedEOFError(sourceLocation common.SourceLocation) UnexpectedEOFError {
	return UnexpectedEOFError{
		ReadError{
			sourceLocation,
			"unexpected EOF",
		},
	}
}
