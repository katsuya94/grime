package read

import (
	"bytes"
	"github.com/katsuya94/grime/common"
	"io"
	"strings"
)

const (
	PARENTHESES = iota
	BRACKETS
)

type DatumReader struct {
	reader *LexemeReader
}

func NewDatumReader(r io.Reader) *DatumReader {
	return &DatumReader{NewLexemeReader(r)}
}

func (d *DatumReader) ReadDatum() (common.Datum, error) {
	lexeme, err := d.reader.ReadLexeme()
	if err != nil {
		return nil, err
	}
	switch v := lexeme.(type) {
	case Boolean:
		return common.Boolean(v), nil
	case Number:
		return common.Number(v), nil
	case Character:
		return common.Character(v), nil
	case String:
		return common.String(v), nil
	case Identifier:
		return common.Symbol(v), nil
	case LeftParenthesis:
		return d.readList(PARENTHESES)
	case LeftBracket:
		return d.readList(BRACKETS)
	case Quote:
		if datum, err := d.ReadDatum(); err == nil {
			return common.Pair{common.Symbol("quote"), common.Pair{datum, nil}}, nil
		} else {
			return nil, err
		}
	case Quasiquote:
		if datum, err := d.ReadDatum(); err == nil {
			return common.Pair{common.Symbol("quasiquote"), common.Pair{datum, nil}}, nil
		} else {
			return nil, err
		}
	case Unquote:
		if datum, err := d.ReadDatum(); err == nil {
			return common.Pair{common.Symbol("unquote"), common.Pair{datum, nil}}, nil
		} else {
			return nil, err
		}
	case UnquoteSplicing:
		if datum, err := d.ReadDatum(); err == nil {
			return common.Pair{common.Symbol("unquote-splicing"), common.Pair{datum, nil}}, nil
		} else {
			return nil, err
		}
	case Syntax:
		if datum, err := d.ReadDatum(); err == nil {
			return common.Pair{common.Symbol("syntax"), common.Pair{datum, nil}}, nil
		} else {
			return nil, err
		}
	case Quasisyntax:
		if datum, err := d.ReadDatum(); err == nil {
			return common.Pair{common.Symbol("quasisyntax"), common.Pair{datum, nil}}, nil
		} else {
			return nil, err
		}
	case Unsyntax:
		if datum, err := d.ReadDatum(); err == nil {
			return common.Pair{common.Symbol("unsyntax"), common.Pair{datum, nil}}, nil
		} else {
			return nil, err
		}
	case UnsyntaxSplicing:
		if datum, err := d.ReadDatum(); err == nil {
			return common.Pair{common.Symbol("unsyntax-splicing"), common.Pair{datum, nil}}, nil
		} else {
			return nil, err
		}
	default:
		return nil, Errorf("unexpected lexeme: %#v", lexeme)
	}
}

func (d *DatumReader) expectDatum() (common.Datum, error) {
	if datum, err := d.ReadDatum(); err == io.EOF {
		return nil, ErrUnexpectedEOF
	} else if err != nil {
		return nil, err
	} else {
		return datum, nil
	}
}

func (d *DatumReader) readList(kind int) (common.Datum, error) {
	for {
		lexeme, err := d.expectNonEOF()
		if err != nil {
			return nil, err
		}
		switch lexeme.(type) {
		case RightParenthesis:
			if kind == PARENTHESES {
				return nil, nil
			} else {
				return nil, Errorf("unexpected lexeme: %#v", lexeme)
			}
		case RightBracket:
			if kind == BRACKETS {
				return nil, nil
			} else {
				return nil, Errorf("unexpected lexeme: %#v", lexeme)
			}
		case Dot:
			datum, err := d.expectDatum()
			if err != nil {
				return nil, err
			}
			lexeme, err := d.expectNonEOF()
			if err != nil {
				return nil, err
			}
			switch lexeme.(type) {
			case RightParenthesis:
				if kind != PARENTHESES {
					return nil, Errorf("unexpected lexeme: %#v", lexeme)
				}
			case RightBracket:
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
			return common.Pair{first, rest}, nil
		}
	}
}

func (d *DatumReader) expectNonEOF() (Lexeme, error) {
	if lexeme, err := d.reader.ReadLexeme(); err == io.EOF {
		return nil, ErrUnexpectedEOF
	} else if err == ErrUnexpectedEOF {
		return nil, ErrUnexpectedEOF
	} else if err != nil {
		return nil, err
	} else {
		return lexeme, nil
	}
}

func Read(r io.Reader) ([]common.Datum, error) {
	datumReader := NewDatumReader(r)
	var data []common.Datum
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

func ReadString(s string) ([]common.Datum, error) {
	return Read(strings.NewReader(s))
}

func MustReadString(s string) []common.Datum {
	data, err := ReadString(s)
	if err != nil {
		panic(err)
	}
	return data
}

func ReadBytes(b []byte) ([]common.Datum, error) {
	return Read(bytes.NewReader(b))
}
