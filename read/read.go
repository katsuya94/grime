package read

import(
	"github.com/katsuya94/grime/lex"
	"io"
	"strings"
	"fmt"
)

type Datum interface{}

type Identifier string

type DatumReader struct {
	lexemeReader *lex.LexemeReader
}

func (d *DatumReader) ReadDatum() (Datum, error) {
	lexeme, err := d.lexemeReader.ReadLexeme()
	if err != nil {
		return nil, err
	}
	switch v := lexeme.(type) {
	case lex.Identifier:
		return Identifier(v), nil
	default:
		return nil, fmt.Errorf("unhandled: %#v", lexeme)
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
