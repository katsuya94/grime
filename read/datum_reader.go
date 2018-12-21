package read

import (
	"bytes"
	"fmt"
	"io"
	"strings"

	"github.com/katsuya94/grime/common"
)

const (
	PARENTHESES = iota
	BRACKETS
)

type DatumReader struct {
	reader *LexemeReader
}

func NewDatumReader(filename string, r io.Reader) *DatumReader {
	return &DatumReader{NewLexemeReader(filename, r)}
}

func (d *DatumReader) ReadDatum() (common.Datum, common.SourceLocationTree, error) {
	lexeme, sourceLocation, err := d.reader.ReadLexeme()
	if err != nil {
		return nil, common.SourceLocationTree{}, err
	}
	switch v := lexeme.(type) {
	case Boolean:
		return common.Boolean(v), common.SourceLocationTree{sourceLocation, nil}, nil
	case Number:
		return common.Number(v), common.SourceLocationTree{sourceLocation, nil}, nil
	case Character:
		return common.Character(v), common.SourceLocationTree{sourceLocation, nil}, nil
	case String:
		return common.String(v), common.SourceLocationTree{sourceLocation, nil}, nil
	case Identifier:
		return common.Symbol(v), common.SourceLocationTree{sourceLocation, nil}, nil
	case LeftParenthesis:
		datum, sourceLocationTree, err := d.readList(PARENTHESES)
		if err != nil {
			return nil, common.SourceLocationTree{}, err
		}
		return datum, listSourceLocationTree(sourceLocation, sourceLocationTree), nil
	case LeftBracket:
		datum, sourceLocationTree, err := d.readList(BRACKETS)
		if err != nil {
			return nil, common.SourceLocationTree{}, err
		}
		return datum, listSourceLocationTree(sourceLocation, sourceLocationTree), nil
	case Quote:
		datum, sourceLocationTree, err := d.ReadDatum()
		if err != nil {
			return nil, common.SourceLocationTree{}, err
		}
		// TODO: consider introducing a special kind of wrapped syntax so this always resolves
		return common.Pair{common.Symbol("quote"), common.Pair{datum, common.Null}}, abbreviationSourceLocationTree(sourceLocation, sourceLocationTree), nil
	case Quasiquote:
		datum, sourceLocationTree, err := d.ReadDatum()
		if err != nil {
			return nil, common.SourceLocationTree{}, err
		}
		return common.Pair{common.Symbol("quasiquote"), common.Pair{datum, common.Null}}, abbreviationSourceLocationTree(sourceLocation, sourceLocationTree), nil
	case Unquote:
		datum, sourceLocationTree, err := d.ReadDatum()
		if err != nil {
			return nil, common.SourceLocationTree{}, err
		}
		return common.Pair{common.Symbol("unquote"), common.Pair{datum, common.Null}}, abbreviationSourceLocationTree(sourceLocation, sourceLocationTree), nil
	case UnquoteSplicing:
		datum, sourceLocationTree, err := d.ReadDatum()
		if err != nil {
			return nil, common.SourceLocationTree{}, err
		}
		return common.Pair{common.Symbol("unquote-splicing"), common.Pair{datum, common.Null}}, abbreviationSourceLocationTree(sourceLocation, sourceLocationTree), nil
	case Syntax:
		datum, sourceLocationTree, err := d.ReadDatum()
		if err != nil {
			return nil, common.SourceLocationTree{}, err
		}
		return common.Pair{common.Symbol("syntax"), common.Pair{datum, common.Null}}, abbreviationSourceLocationTree(sourceLocation, sourceLocationTree), nil
	case Quasisyntax:
		datum, sourceLocationTree, err := d.ReadDatum()
		if err != nil {
			return nil, common.SourceLocationTree{}, err
		}
		return common.Pair{common.Symbol("quasisyntax"), common.Pair{datum, common.Null}}, abbreviationSourceLocationTree(sourceLocation, sourceLocationTree), nil
	case Unsyntax:
		datum, sourceLocationTree, err := d.ReadDatum()
		if err != nil {
			return nil, common.SourceLocationTree{}, err
		}
		return common.Pair{common.Symbol("unsyntax"), common.Pair{datum, common.Null}}, abbreviationSourceLocationTree(sourceLocation, sourceLocationTree), nil
	case UnsyntaxSplicing:
		datum, sourceLocationTree, err := d.ReadDatum()
		if err != nil {
			return nil, common.SourceLocationTree{}, err
		}
		return common.Pair{common.Symbol("unsyntax-splicing"), common.Pair{datum, common.Null}}, abbreviationSourceLocationTree(sourceLocation, sourceLocationTree), nil
	default:
		return nil, common.SourceLocationTree{}, ReadError{sourceLocation, fmt.Sprintf("unexpected lexeme: #<%T>", v)}
	}
}

func listSourceLocationTree(sourceLocation common.SourceLocation, sourceLocationTree common.SourceLocationTree) common.SourceLocationTree {
	return common.SourceLocationTree{
		common.SourceLocation{
			File:   sourceLocation.File,
			Line:   sourceLocation.Line,
			Column: sourceLocation.Column,
			Offset: sourceLocation.Offset,
			Length: (sourceLocationTree.Offset + sourceLocationTree.Length) - sourceLocation.Offset,
		},
		sourceLocationTree,
	}
}

func abbreviationSourceLocationTree(sourceLocation common.SourceLocation, sourceLocationTree common.SourceLocationTree) common.SourceLocationTree {
	return common.SourceLocationTree{
		sourceLocation,
		common.Pair{
			common.SourceLocationTree{
				sourceLocation,
				nil,
			},
			common.SourceLocationTree{
				sourceLocation,
				common.Pair{
					sourceLocationTree,
					common.SourceLocationTree{
						sourceLocation,
						nil,
					},
				},
			},
		},
	}
}

func (d *DatumReader) expectDatum() (common.Datum, common.SourceLocationTree, error) {
	datum, sourceLocationTree, err := d.ReadDatum()
	if err == io.EOF {
		return nil, common.SourceLocationTree{}, NewUnexpectedEOFError(d.reader.sourceLocation())
	} else if err != nil {
		return nil, common.SourceLocationTree{}, err
	}
	return datum, sourceLocationTree, nil
}

func (d *DatumReader) readList(kind int) (common.Datum, common.SourceLocationTree, error) {
	for {
		lexeme, sourceLocation, err := d.expectNonEOF()
		if err != nil {
			return nil, common.SourceLocationTree{}, err
		}
		switch lexeme.(type) {
		case RightParenthesis:
			if kind == PARENTHESES {
				return common.Null, common.SourceLocationTree{sourceLocation, nil}, nil
			} else {
				return nil, common.SourceLocationTree{}, ReadError{sourceLocation, fmt.Sprintf("unexpected lexeme: #<%T>", lexeme)}
			}
		case RightBracket:
			if kind == BRACKETS {
				return common.Null, common.SourceLocationTree{sourceLocation, nil}, nil
			} else {
				return nil, common.SourceLocationTree{}, ReadError{sourceLocation, fmt.Sprintf("unexpected lexeme: #<%T>", lexeme)}
			}
		case Dot:
			datum, sourceLocationTree, err := d.expectDatum()
			if err != nil {
				return nil, common.SourceLocationTree{}, err
			}
			lexeme, sourceLocation, err := d.expectNonEOF()
			if err != nil {
				return nil, common.SourceLocationTree{}, err
			}
			switch lexeme.(type) {
			case RightParenthesis:
				if kind != PARENTHESES {
					return nil, common.SourceLocationTree{}, ReadError{sourceLocation, fmt.Sprintf("unexpected lexeme: #<%T>", lexeme)}
				}
			case RightBracket:
				if kind != BRACKETS {
					return nil, common.SourceLocationTree{}, ReadError{sourceLocation, fmt.Sprintf("unexpected lexeme: #<%T>", lexeme)}
				}
			default:
				return nil, common.SourceLocationTree{}, ReadError{sourceLocation, fmt.Sprintf("unexpected lexeme: #<%T>", lexeme)}
			}
			return datum, sourceLocationTree, nil
		default:
			d.reader.UnreadLexeme()
			first, firstSourceLocationTree, err := d.expectDatum()
			if err != nil {
				return nil, common.SourceLocationTree{}, err
			}
			rest, restSourceLocationTree, err := d.readList(kind)
			if err != nil {
				return nil, common.SourceLocationTree{}, err
			}
			return common.Pair{first, rest}, listSourceLocationTree(firstSourceLocationTree.SourceLocation, restSourceLocationTree), nil
		}
	}
}

func (d *DatumReader) expectNonEOF() (Lexeme, common.SourceLocation, error) {
	lexeme, sourceLocation, err := d.reader.ReadLexeme()
	if err == io.EOF {
		return nil, common.SourceLocation{}, NewUnexpectedEOFError(d.reader.sourceLocation())
	} else if err != nil {
		return nil, common.SourceLocation{}, err
	}
	return lexeme, sourceLocation, nil
}

func Read(filename string, r io.Reader) ([]common.Datum, []common.SourceLocationTree, error) {
	datumReader := NewDatumReader(filename, r)
	var (
		data                []common.Datum
		sourceLocationTrees []common.SourceLocationTree
	)
	for {
		datum, sourceLocationTree, err := datumReader.ReadDatum()
		if err == io.EOF {
			break
		} else if err != nil {
			return nil, nil, err
		}
		data = append(data, datum)
		sourceLocationTrees = append(sourceLocationTrees, sourceLocationTree)
	}
	return data, sourceLocationTrees, nil
}

func ReadString(s string) ([]common.Datum, []common.SourceLocationTree, error) {
	return Read("string", strings.NewReader(s))
}

func MustReadData(s string) []common.Datum {
	data, _, err := ReadString(s)
	if err != nil {
		panic(err)
	}
	return data
}

func MustReadDatum(s string) common.Datum {
	data := MustReadData(s)
	if len(data) != 1 {
		panic(fmt.Sprintf("encountered %v data", len(data)))
	}
	return data[0]
}

func ReadBytes(b []byte) ([]common.Datum, []common.SourceLocationTree, error) {
	return Read("bytes", bytes.NewReader(b))
}
