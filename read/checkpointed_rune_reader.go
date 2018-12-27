package read

import (
	"bufio"
	"io"
)

type CheckpointedRuneReader struct {
	r      *bufio.Reader
	buf    []rune
	i      int
	eof    bool
	line   int
	column int
	offset int
}

func NewCheckpointedRuneReader(r io.Reader) *CheckpointedRuneReader {
	return &CheckpointedRuneReader{bufio.NewReader(r), nil, 0, false, 0, 0, 0}
}

func (c *CheckpointedRuneReader) ReadRune() (rune, error) {
	var (
		r   rune
		err error
	)
	if c.i < len(c.buf) {
		r = c.buf[c.i]
	} else if c.eof {
		err = io.EOF
	} else {
		r, _, err = c.r.ReadRune()
		if err == io.EOF {
			c.eof = true
		} else {
			c.buf = append(c.buf, r)
		}
	}
	c.i++
	return r, err
}

func (c *CheckpointedRuneReader) UnreadRune() {
	if c.i <= 0 {
		panic("invalid use of UnreadRune")
	}
	c.i--
}

func (c *CheckpointedRuneReader) Checkpoint() {
	c.line = c.Line()
	c.column = c.Column()
	c.offset = c.Offset()
	c.buf = c.buf[c.i:]
	c.i = 0
}

func (c *CheckpointedRuneReader) Return() {
	c.i = 0
}

func (c *CheckpointedRuneReader) Line() int {
	line := c.line
	n := c.i
	if n > len(c.buf) {
		n = len(c.buf)
	}
	for j := 0; j < n; j++ {
		if c.buf[j] == '\n' {
			line++
		}
	}
	return line
}

func (c *CheckpointedRuneReader) Column() int {
	column := c.column
	n := c.i
	if n > len(c.buf) {
		n = len(c.buf)
	}
	for j := 0; j < n; j++ {
		if c.buf[j] == '\n' {
			column = 0
		} else {
			column++
		}
	}
	column += c.i - n
	return column
}

func (c *CheckpointedRuneReader) Offset() int {
	return c.offset + c.i
}
