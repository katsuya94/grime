package read

import "fmt"

func Errorf(format string, a ...interface{}) error {
	return fmt.Errorf("read: "+format, a...)
}

var ErrUnexpectedEOF = Errorf("unexpected EOF")

type ReadError struct {
	SourceLocation
	message string
}

func (e ReadError) Error() string {
	return fmt.Sprintf("read: at %v:%v:%v: %v", e.File, e.Line+1, e.Column+1, e.message)
}
