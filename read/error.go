package read

import "fmt"

func Errorf(format string, a ...interface{}) error {
	return fmt.Errorf("read: "+format, a...)
}

var ErrUnexpectedEOF = Errorf("unexpected EOF")
