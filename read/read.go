package read

import (
	"fmt"
	"github.com/katsuya94/grime/lex"
	"io"
	"strings"
)

type Datum interface{}

type Boolean bool
type Number string
type Character rune
type String string
type Symbol string
type Pair struct {
	first Datum
	rest  Datum
}

const (
	PARENTHESES = iota
	BRACKETS
)

type DatumReader struct {
	lexemeReader *lex.LexemeReader
}

func (d *DatumReader) readList(kind int) (Datum, error) {
	for {
		lexeme, err := d.lexemeReader.ReadLexeme()
		if err == io.EOF {
			return nil, fmt.Errorf("unexpected EOF")
		} else if err != nil {
			return nil, err
		}
		switch lexeme.(type) {
		case lex.RightParenthesis:
			if kind == PARENTHESES {
				return nil, nil
			} else {
				return nil, fmt.Errorf("unexpected lexeme: %#v", lexeme)
			}
		case lex.RightBracket:
			if kind == BRACKETS {
				return nil, nil
			} else {
				return nil, fmt.Errorf("unexpected lexeme: %#v", lexeme)
			}
		case lex.Dot:
			datum, err := d.ReadDatum()
			if err == io.EOF {
				return nil, fmt.Errorf("unexpected EOF")
			} else if err != nil {
				return nil, err
			}
			lexeme, err := d.lexemeReader.ReadLexeme()
			if err == io.EOF {
				return nil, fmt.Errorf("unexpected EOF")
			} else if err != nil {
				return nil, err
			}
			switch lexeme.(type) {
			case lex.RightParenthesis:
				if kind != PARENTHESES {
					return nil, fmt.Errorf("unexpected lexeme: %#v", lexeme)
				}
			case lex.RightBracket:
				if kind != BRACKETS {
					return nil, fmt.Errorf("unexpected lexeme: %#v", lexeme)
				}
			default:
				return nil, fmt.Errorf("unexpected lexeme: %#v", lexeme)
			}
			return datum, nil
		default:
			d.lexemeReader.UnreadLexeme()
			first, err := d.ReadDatum()
			if err != nil {
				return nil, err
			}
			rest, err := d.readList(kind)
			if err != nil {
				return nil, err
			}
			return Pair{first, rest}, nil
		}
	}
}

func (d *DatumReader) ReadDatum() (Datum, error) {
	lexeme, err := d.lexemeReader.ReadLexeme()
	if err != nil {
		return nil, err
	}
	switch v := lexeme.(type) {
	case lex.Boolean:
		return Boolean(v), nil
	case lex.Number:
		return Number(v), nil
	case lex.Character:
		return Character(v), nil
	case lex.String:
		return String(v), nil
	case lex.Identifier:
		return Symbol(v), nil
	case lex.LeftParenthesis:
		return d.readList(PARENTHESES)
	case lex.LeftBracket:
		return d.readList(BRACKETS)
	case lex.Quote:
		if datum, err := d.ReadDatum(); err == nil {
			return Pair{Symbol("quote"), Pair{datum, nil}}, nil
		} else {
			return nil, err
		}
	case lex.QuasiQuote:
		if datum, err := d.ReadDatum(); err == nil {
			return Pair{Symbol("quasiquote"), Pair{datum, nil}}, nil
		} else {
			return nil, err
		}
	case lex.Unquote:
		if datum, err := d.ReadDatum(); err == nil {
			return Pair{Symbol("unquote"), Pair{datum, nil}}, nil
		} else {
			return nil, err
		}
	case lex.UnquoteSplicing:
		if datum, err := d.ReadDatum(); err == nil {
			return Pair{Symbol("unquote-splicing"), Pair{datum, nil}}, nil
		} else {
			return nil, err
		}
	default:
		return nil, fmt.Errorf("unexpected lexeme: %#v", lexeme)
	}
}

func NewDatumReader(r io.Reader) *DatumReader {
	return &DatumReader{lex.NewLexemeReader(r)}
}

func Read(r io.Reader) ([]Datum, error) {
	datumReader := NewDatumReader(r)
	var data []Datum
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

func ReadString(s string) ([]Datum, error) {
	return Read(strings.NewReader(s))
}
