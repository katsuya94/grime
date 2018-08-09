package lex

import (
	"bufio"
	"fmt"
	"io"
)

type CheckpointedRuneReader struct {
	r   *bufio.Reader
	buf []rune
	i   int
	eof bool
}

func NewCheckpointedRuneReader(r io.Reader) *CheckpointedRuneReader {
	return &CheckpointedRuneReader{bufio.NewReader(r), nil, 0, false}
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
	c.i += 1
	return r, err
}

func (c *CheckpointedRuneReader) UnreadRune() error {
	if c.i <= 0 {
		return fmt.Errorf("lex: invalid use of UnreadRune")
	}
	c.i -= 1
	return nil
}

func (c *CheckpointedRuneReader) Checkpoint() {
	c.buf = c.buf[c.i:]
	c.i = 0
}

func (c *CheckpointedRuneReader) Return() {
	c.i = 0
}

func (c *CheckpointedRuneReader) Consumed() []rune {
	return c.buf[:c.i]
}
