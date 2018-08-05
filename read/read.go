package read

import(
	"github.com/katsuya94/grime/lex"
	"io"
	"strings"
	"fmt"
)

type Datum interface{}

type Boolean bool
type Number string
type Character rune
type String string
type Symbol string

const (
	PARENTHESES = iota
	BRACKET
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
		switch v := lexeme.(type) {
		case lex.RightParenthesis:
			if kind == PARENTHESES {
				return nil, nil
			} else {
				return nil, fmt.Errorf("unexpected lexeme: %#v", lexeme)
			}
		case lex.RightBracket:
			if kind == BRACKET {
				return nil, nil
			} else {
				return nil, fmt.Errorf("unexpected lexeme: %#v", lexeme)
			}
		case lex.Dot:

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
		return d.readList(BRACKET)
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
