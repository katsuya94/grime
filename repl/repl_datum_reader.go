package repl

import (
	"github.com/katsuya94/grime/core"
	"io"
	"bufio"
	"bytes"
	"github.com/katsuya94/grime/read"
)

const (
	line = iota
	interrupt
	eof
	datum
	recoverableError
	unrecoverableError
)

type replDatumReaderEvent struct {
	kind int
	value interface{}
}

type REPLDatumReader struct{
	c chan replDatumReaderEvent
}

func NewREPLDatumReader() (*REPLDatumReader) {
	return &REPLDatumReader{
		make(chan replDatumReaderEvent),
	}
}

func (r *REPLDatumReader) Start(reader io.Reader) {
	r.scanLines(reader)
}

func (r *REPLDatumReader) ReadDatum() (core.Datum, bool, error) {
	var buf []byte
	for {
		switch e := <- r.c; e.kind {
		case line:
			buf = append(buf, e.value.([]byte)...)
			go r.readData(buf)
		case interrupt:
		case eof:
		case datum:
			return e.value.(core.Datum), true, nil
		case recoverableError:
			return nil, true, e.value.(error)
		case unrecoverableError:
			return nil, false, e.value.(error)
		}
	}
}

func (r *REPLDatumReader) scanLines(reader io.Reader) {
	scanner := bufio.NewScanner(reader)
	scanner.Split(splitReplLines)
	go func() {
		for {
			if scanner.Scan() {
				r.c <- replDatumReaderEvent{line, scanner.Bytes()}
			} else if err := scanner.Err(); err != nil {
				r.c <- replDatumReaderEvent{unrecoverableError, err}
				return
			} else {
				r.c <- replDatumReaderEvent{line, scanner.Bytes()}
				r.c <- replDatumReaderEvent{eof, nil}
				return
			}
		}
	}()
}

func (r *REPLDatumReader) readData(buf []byte) {
	reader := read.NewDatumReader(bytes.NewReader(buf))
	for {
		if d, err := reader.ReadDatum(); err == io.EOF {
			break
		} else if err != nil {
			r.c <- replDatumReaderEvent{recoverableError, err}
			break
		} else {
			r.c <- replDatumReaderEvent{datum, d}
		}
	}
}

func splitReplLines(data []byte, atEOF bool) (int, []byte, error) {
	if atEOF && len(data) == 0 {
		return 0, nil, nil
	}
	if i := bytes.IndexByte(data, '\n'); i >= 0 {
		return i + 1, data[0 : i+1], nil
	}
	if atEOF {
		return len(data), data, nil

	}
	return 0, nil, nil
}
