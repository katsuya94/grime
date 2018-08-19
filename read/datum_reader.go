package read

import (
	"bytes"
	"fmt"
	"github.com/katsuya94/grime/core"
	"github.com/katsuya94/grime/lex"
	"io"
	"strings"
)

func Errorf(format string, a ...interface{}) error {
	return fmt.Errorf("read: "+format, a...)
}

var ErrUnexpectedEOF = Errorf("unexpected EOF")

const (
	PARENTHESES = iota
	BRACKETS
)

type DatumReader struct {
	reader *lex.LexemeReader
}

func NewDatumReader(r io.Reader) *DatumReader {
	return &DatumReader{lex.NewLexemeReader(r)}
}

func (d *DatumReader) ReadDatum() (core.Datum, error) {
	lexeme, err := d.reader.ReadLexeme()
	if err == lex.ErrUnexpectedEOF {
		return nil, ErrUnexpectedEOF
	} else if err != nil {
		return nil, err
	}
	switch v := lexeme.(type) {
	case lex.Boolean:
		return core.Boolean(v), nil
	case lex.Number:
		return core.Number(v), nil
	case lex.Character:
		return core.Character(v), nil
	case lex.String:
		return core.String(v), nil
	case lex.Identifier:
		return core.Symbol(v), nil
	case lex.LeftParenthesis:
		return d.readList(PARENTHESES)
	case lex.LeftBracket:
		return d.readList(BRACKETS)
	case lex.Quote:
		if datum, err := d.ReadDatum(); err == nil {
			return core.Pair{core.Symbol("quote"), core.Pair{datum, nil}}, nil
		} else {
			return nil, err
		}
	case lex.Quasiquote:
		if datum, err := d.ReadDatum(); err == nil {
			return core.Pair{core.Symbol("quasiquote"), core.Pair{datum, nil}}, nil
		} else {
			return nil, err
		}
	case lex.Unquote:
		if datum, err := d.ReadDatum(); err == nil {
			return core.Pair{core.Symbol("unquote"), core.Pair{datum, nil}}, nil
		} else {
			return nil, err
		}
	case lex.UnquoteSplicing:
		if datum, err := d.ReadDatum(); err == nil {
			return core.Pair{core.Symbol("unquote-splicing"), core.Pair{datum, nil}}, nil
		} else {
			return nil, err
		}
	case lex.Syntax:
		if datum, err := d.ReadDatum(); err == nil {
			return core.Pair{core.Symbol("syntax"), core.Pair{datum, nil}}, nil
		} else {
			return nil, err
		}
	case lex.Quasisyntax:
		if datum, err := d.ReadDatum(); err == nil {
			return core.Pair{core.Symbol("quasisyntax"), core.Pair{datum, nil}}, nil
		} else {
			return nil, err
		}
	case lex.Unsyntax:
		if datum, err := d.ReadDatum(); err == nil {
			return core.Pair{core.Symbol("unsyntax"), core.Pair{datum, nil}}, nil
		} else {
			return nil, err
		}
	case lex.UnsyntaxSplicing:
		if datum, err := d.ReadDatum(); err == nil {
			return core.Pair{core.Symbol("unsyntax-splicing"), core.Pair{datum, nil}}, nil
		} else {
			return nil, err
		}
	default:
		return nil, Errorf("unexpected lexeme: %#v", lexeme)
	}
}

func (d *DatumReader) expectDatum() (core.Datum, error) {
	if datum, err := d.ReadDatum(); err == io.EOF {
		return nil, ErrUnexpectedEOF
	} else if err != nil {
		return nil, err
	} else {
		return datum, nil
	}
}

func (d *DatumReader) readList(kind int) (core.Datum, error) {
	for {
		lexeme, err := d.expectNonEOF()
		if err != nil {
			return nil, err
		}
		switch lexeme.(type) {
		case lex.RightParenthesis:
			if kind == PARENTHESES {
				return nil, nil
			} else {
				return nil, Errorf("unexpected lexeme: %#v", lexeme)
			}
		case lex.RightBracket:
			if kind == BRACKETS {
				return nil, nil
			} else {
				return nil, Errorf("unexpected lexeme: %#v", lexeme)
			}
		case lex.Dot:
			datum, err := d.expectDatum()
			if err != nil {
				return nil, err
			}
			lexeme, err := d.expectNonEOF()
			if err != nil {
				return nil, err
			}
			switch lexeme.(type) {
			case lex.RightParenthesis:
				if kind != PARENTHESES {
					return nil, Errorf("unexpected lexeme: %#v", lexeme)
				}
			case lex.RightBracket:
				if kind != BRACKETS {
					return nil, Errorf("unexpected lexeme: %#v", lexeme)
				}
			default:
				return nil, Errorf("unexpected lexeme: %#v", lexeme)
			}
			return datum, nil
		default:
			d.reader.UnreadLexeme()
			first, err := d.expectDatum()
			if err != nil {
				return nil, err
			}
			rest, err := d.readList(kind)
			if err != nil {
				return nil, err
			}
			return core.Pair{first, rest}, nil
		}
	}
}

func (d *DatumReader) expectNonEOF() (lex.Lexeme, error) {
	if lexeme, err := d.reader.ReadLexeme(); err == io.EOF {
		return nil, ErrUnexpectedEOF
	} else if err == lex.ErrUnexpectedEOF {
		return nil, ErrUnexpectedEOF
	} else if err != nil {
		return nil, err
	} else {
		return lexeme, nil
	}
}

func Read(r io.Reader) ([]core.Datum, error) {
	datumReader := NewDatumReader(r)
	var data []core.Datum
	for {
		if datum, err := datumReader.ReadDatum(); err == nil {
			data = append(data, datum)
		} else if err == io.EOF {
			return data, nil
		} else {
			return nil, err
		}
	}
}

func ReadString(s string) ([]core.Datum, error) {
	return Read(strings.NewReader(s))
}

func MustReadString(s string) []core.Datum {
	data, err := ReadString(s)
	if err != nil {
		panic(err)
	}
	return data
}

func ReadBytes(b []byte) ([]core.Datum, error) {
	return Read(bytes.NewReader(b))
}
