package lex

import (
	"fmt"
	"io"
	"strings"
)

type LexemeReader struct {
	reader *CheckpointedRuneReader
	next   Lexeme
	unread bool
}

func NewLexemeReader(r io.Reader) *LexemeReader {
	return &LexemeReader{
		NewCheckpointedRuneReader(r),
		nil,
		false,
	}
}

func (l *LexemeReader) ReadLexeme() (Lexeme, error) {
	if l.unread {
		l.unread = false
		return l.next, nil
	}
	if err := l.readInterlexemeSpace(); err != nil {
		return nil, err
	}
	l.reader.Checkpoint()
	lexeme, err := l.readLexeme()
	if err == io.EOF {
		return nil, fmt.Errorf("unexpected EOF")
	} else if err != nil {
		return nil, err
	}
	l.next = lexeme
	return lexeme, nil
}

func (l *LexemeReader) readInterlexemeSpace() error {
	for {
		if r, err := l.reader.ReadRune(); (!whitespace(r) && err == nil) || err == io.EOF {
			l.reader.UnreadRune()
			return err
		} else if err != nil {
			return err
		}
	}
}

func (l *LexemeReader) readLexeme() (Lexeme, error) {
	if lexeme, ok, err := l.readIdentifier(); err != nil {
		return nil, err
	} else if ok {
		return lexeme, nil
	}
	if boolean, err := l.readBoolean(); err != nil && err != io.EOF {
		return nil, err
	} else if boolean != nil {
		return boolean, err
	}
	if number, err := l.readNumber(); err != nil && err != io.EOF {
		return nil, err
	} else if number != nil {
		return number, err
	}
	if character, err := l.readCharacter(); err != nil && err != io.EOF {
		return nil, err
	} else if character != nil {
		return character, err
	}
	if string, err := l.readString(); err != nil && err != io.EOF {
		return nil, err
	} else if string != nil {
		return string, err
	}
	r, err := l.reader.ReadRune()
	if err == io.EOF {
		return nil, fmt.Errorf("unexpected EOF")
	} else if err != nil {
		return nil, err
	}
	switch r {
	case '(':
		return LeftParenthesis{}, nil
	case ')':
		return RightParenthesis{}, nil
	case '[':
		return LeftBracket{}, nil
	case ']':
		return RightBracket{}, nil
	case '\'':
		return Quote{}, nil
	case '`':
		return QuasiQuote{}, nil
	case ',':
		if err := l.nextExact('@'); err == nil {
			return UnquoteSplicing{}, nil
		} else {
			l.reader.UnreadRune()
			return Unquote{}, nil
		}
	case '.':
		return Dot{}, nil
	default:
		return nil, fmt.Errorf("unexpected rune: %#v", string(r))
	}
}

func (l *LexemeReader) readIdentifier() (Lexeme, bool, error) {
	if r, ok, err := l.readNonDelimiter(); err != nil {
		return nil, false, err
	} else if !ok {
		return nil, false, nil
	} else if !initial(r) {
		l.reader.Return()
		return l.readPeculiarIdentifier()
	} else {
		return l.readSubsequent([]rune{r})
	}
}

func (l *LexemeReader) readPeculiarIdentifier() (Lexeme, bool, error) {
	r, ok, err := l.readNonDelimiter()
	if err != nil {
		return nil, false, err
	} else if !ok {
		return nil, false, nil
	}
	switch r {
	case '+':
		if _, ok, err := l.readNonDelimiter(); err != nil {
			return nil, false, err
		} else if ok {
			return nil, false, fmt.Errorf("expected delimiter")
		}
		return Identifier("+"), true, nil
	case '-':
		if r, ok, err := l.readNonDelimiter(); err != nil {
			return nil, false, err
		} else if !ok {
			return Identifier("-"), true, nil
		} else if r != '>' {
			return nil, false, fmt.Errorf("expected delimiter or >")
		}
		return l.readSubsequent([]rune{'-', '>'})
	case '.':
		if r, ok, err := l.readNonDelimiter(); err != nil {
			return nil, false, err
		} else if !ok {
			l.reader.Return()
			return nil, false, nil
		} else if r != '.' {
			return nil, false, fmt.Errorf("expected .")
		}
		if r, ok, err := l.readNonDelimiter(); err != nil {
			return nil, false, err
		} else if !ok {
			return nil, false, fmt.Errorf("unexpected delimiter")
		} else if r != '.' {
			return nil, false, fmt.Errorf("expected .")
		}
		if _, ok, err := l.readNonDelimiter(); err != nil {
			return nil, false, err
		} else if ok {
			return nil, false, fmt.Errorf("expected delimiter")
		}
		return Identifier("..."), true, nil
	default:
		l.reader.Return()
		return nil, false, nil
	}
}

func (l *LexemeReader) readSubsequent(prefix []rune) (Lexeme, bool, error) {
	runes := prefix
	for {
		if r, ok, err := l.readNonDelimiter(); err != nil {
			return nil, false, err
		} else if ok {
			runes = append(runes, r)
		} else {
			return Identifier(runes), true, nil
		}
	}
}

func (l *LexemeReader) readNonDelimiter() (rune, bool, error) {
	if r, err := l.reader.ReadRune(); err == io.EOF {
		l.reader.UnreadRune()
		return 0, false, nil
	} else if err != nil {
		return 0, false, err
	} else if delimiter(r) {
		l.reader.UnreadRune()
		return 0, false, nil
	} else {
		return r, true, nil
	}
}

func (l *LexemeReader) nextExpectingNonDelimiter() (rune, error) {
	if r, ok, err := l.readNonDelimiter(); err != nil {
		return 0, err
	} else if ok {
		return r, nil
	} else {
		return 0, fmt.Errorf("unexpected delimiter")
	}
}

func (l *LexemeReader) nextExpectingDelimiter() error {
	if _, ok, err := l.readNonDelimiter(); err != nil {
		return err
	} else if ok {
		return fmt.Errorf("expected delimiter")
	} else {
		return nil
	}
}

func (l *LexemeReader) nextSatisfying(p func(rune) bool) (rune, error) {
	if r, err := l.reader.ReadRune(); p(r) && err == nil {
		return r, nil
	} else if err == nil {
		return 0, io.EOF
	} else {
		return 0, err
	}
}

func (l *LexemeReader) nextExact(e rune) error {
	if r, err := l.reader.ReadRune(); r == e && err == nil {
		return nil
	} else if err == nil {
		return io.EOF
	} else {
		return err
	}
}

func (l *LexemeReader) nextNonDelimiterExpecting(p func(rune) bool) (rune, error) {
	if r, ok, err := l.readNonDelimiter(); err != nil {
		return 0, err
	} else if !ok {
		return 0, io.EOF
	} else if p(r) {
		return r, nil
	} else {
		return 0, fmt.Errorf("unexpected rune")
	}
}

func (l *LexemeReader) nextExactExpectingNonDelimiter(e rune) error {
	if r, ok, err := l.readNonDelimiter(); err != nil {
		return err
	} else if !ok {
		return fmt.Errorf("unexpected delimiter")
	} else if r == e {
		return nil
	} else {
		return fmt.Errorf("unexpected rune")
	}
}

func (l *LexemeReader) nextExpectingExact(e rune) error {
	if r, err := l.reader.ReadRune(); err == io.EOF {
		return fmt.Errorf("unexpected EOF")
	} else if err != nil {
		return err
	} else if r == e {
		return nil
	} else {
		return fmt.Errorf("unexpected rune")
	}
}

func (l *LexemeReader) nextExpecting(p func(rune) bool) (rune, error) {
	if r, err := l.reader.ReadRune(); p(r) && err == nil {
		return r, nil
	} else if err == nil {
		return 0, fmt.Errorf("unexpected rune: %#v", string(r))
	} else if err == io.EOF {
		return 0, fmt.Errorf("unexpected EOF")
	} else {
		return 0, err
	}
}

func (l *LexemeReader) readBoolean() (Lexeme, error) {
	if err := l.nextExact('#'); err == io.EOF {
		l.reader.Return()
		return nil, nil
	} else if err != nil {
		return nil, err
	}
	r, err := l.nextExpectingNonDelimiter()
	if err != nil {
		return nil, err
	}
	var boolean Lexeme
	switch r {
	case 'f':
		boolean = Boolean(false)
	case 'F':
		boolean = Boolean(false)
	case 't':
		boolean = Boolean(true)
	case 'T':
		boolean = Boolean(true)
	default:
		l.reader.Return()
		return nil, nil
	}
	if err := l.nextExpectingDelimiter(); err == nil {
		return boolean, nil
	} else {
		return nil, err
	}
}

func (l *LexemeReader) readNumber() (Lexeme, error) {
	var runes []rune
	if r, err := l.nextSatisfying(digit); err == nil {
		runes = append(runes, r)
	} else if err == io.EOF {
		l.reader.Return()
		return nil, nil
	} else {
		return nil, err
	}
	for {
		if r, err := l.nextNonDelimiterExpecting(digit); err == nil {
			runes = append(runes, r)
		} else if err == io.EOF {
			return Number(runes), nil
		} else {
			return nil, err
		}
	}
}

func hexValue(r rune) rune {
	if '0' <= r && r <= '9' {
		return r - '0'
	} else if 'a' <= r && r <= 'f' {
		return 10 + r - 'a'
	} else if 'A' <= r && r <= 'F' {
		return 10 + r - 'A'
	} else {
		panic(fmt.Sprintf("invalid hex digit: %#v", string(r)))
	}
}

func (l *LexemeReader) readHex() (rune, error) {
	var hexDigits []rune
	if r, err := l.nextExpecting(hexDigit); err == nil {
		hexDigits = append(hexDigits, r)
	} else {
		return 0, err
	}
	for {
		if r, err := l.nextSatisfying(hexDigit); err == nil {
			hexDigits = append(hexDigits, r)
		} else if err == io.EOF {
			l.reader.UnreadRune()
			var (
				i int
				r rune
			)
			for i, r = range hexDigits {
				if r > '0' {
					break
				}
			}
			if len(hexDigits)-i > 8 {
				return 0, fmt.Errorf(`character out of range: \#x%v`, string(hexDigits))
			}
			hexDigits = hexDigits[i:]
			r = 0
			for _, hexDigit := range hexDigits {
				r <<= 4
				r += hexValue(hexDigit)
			}
			return r, nil
		} else {
			return 0, err
		}
	}
}

func (l *LexemeReader) readCharacter() (Lexeme, error) {
	if err := l.nextExact('#'); err == io.EOF {
		l.reader.Return()
		return nil, nil
	} else if err != nil {
		return nil, err
	}
	if err := l.nextExactExpectingNonDelimiter('\\'); err == io.EOF {
		l.reader.Return()
		return nil, nil
	} else if err != nil {
		return nil, err
	}
	first, err := l.nextExpectingNonDelimiter()
	if err != nil {
		return nil, err
	}
	if r, ok, err := l.readNonDelimiter(); err != nil {
		return nil, err
	} else if !ok {
		return Character(first), nil
	} else if first == 'x' {
		l.reader.UnreadRune()
		r, err := l.readHex()
		if err != nil {
			return nil, err
		}
		if err := l.nextExpectingDelimiter(); err != nil {
			return nil, err
		}
		return Character(r), err
	} else {
		runes := []rune{first, r}
		for {
			if r, ok, err := l.readNonDelimiter(); err != nil {
				return nil, err
			} else if ok {
				runes = append(runes, r)
			} else {
				switch string(runes) {
				case "nul":
					return Character('\x00'), nil
				case "alarm":
					return Character('\x07'), nil
				case "backspace":
					return Character('\x08'), nil
				case "tab":
					return Character('\x09'), nil
				case "linefeed":
					return Character('\x0a'), nil
				case "newline":
					return Character('\x0a'), nil
				case "vtab":
					return Character('\x0b'), nil
				case "page":
					return Character('\x0c'), nil
				case "return":
					return Character('\x0d'), nil
				case "esc":
					return Character('\x1b'), nil
				case "space":
					return Character('\x20'), nil
				case "delete":
					return Character('\x7f'), nil
				default:
					return nil, fmt.Errorf(`unrecognized character: #\%v`, string(runes))
				}
			}
		}
	}

}

func (l *LexemeReader) readLineEnding() (bool, error) {
	r, err := l.reader.ReadRune()
	if err == io.EOF {
		return false, fmt.Errorf("unexpected EOF")
	} else if err != nil {
		return false, err
	}
	switch r {
	case '\x0a':
		return true, nil
	case '\x0d':
		r, err := l.reader.ReadRune()
		if err == io.EOF {
			l.reader.UnreadRune()
			return true, nil
		} else if err != nil {
			return false, err
		}
		switch r {
		case '\x0a':
			return true, nil
		case '\x85':
			return true, nil
		default:
			l.reader.UnreadRune()
			return true, nil
		}
	case '\x85':
		return true, nil
	case '\u2028':
		return true, nil
	default:
		return false, nil
	}
}

func (l *LexemeReader) readString() (Lexeme, error) {
	if err := l.nextExact('"'); err == io.EOF {
		l.reader.Return()
		return nil, nil
	} else if err != nil {
		return nil, err
	}
	var runes []rune
	for {
		if r, err := l.reader.ReadRune(); r == '"' && err == nil {
			return String(string(runes)), nil
		} else if r == '\\' && err == nil {
			r, err := l.reader.ReadRune()
			if err == io.EOF {
				return nil, fmt.Errorf("unexpected EOF")
			} else if err != nil {
				return nil, err
			}
			nothing := false
			switch r {
			case 'a':
				r = '\x07'
			case 'b':
				r = '\x08'
			case 't':
				r = '\x09'
			case 'n':
				r = '\x0a'
			case 'v':
				r = '\x0b'
			case 'f':
				r = '\x0c'
			case 'r':
				r = '\x0d'
			case '"':
			case '\\':
			case 'x':
				r, err = l.readHex()
				if err != nil {
					return nil, err
				}
				if err := l.nextExpectingExact(';'); err != nil {
					return nil, err
				}
			default:
				if intralineWhitespace(r) {
					if ok, err := l.readLineEnding(); err != nil {
						return nil, err
					} else if !ok {
						return nil, fmt.Errorf("expected line ending")
					}
					if _, err := l.nextExpecting(intralineWhitespace); err == nil {
						nothing = true
					} else {
						return nil, err
					}
				} else {
					return nil, fmt.Errorf(`unrecognized escape pattern: \%v`, r)
				}
			}
			if !nothing {
				runes = append(runes, r)
			}
		} else if err == nil {
			l.reader.UnreadRune()
			if ok, err := l.readLineEnding(); err != nil {
				return nil, err
			} else if ok {
				runes = append(runes, '\x0a')
			} else {
				runes = append(runes, r)
			}
		} else if err == io.EOF {
			return nil, fmt.Errorf("unexpected EOF")
		} else {
			return nil, err
		}
	}
}

func (l *LexemeReader) UnreadLexeme() error {
	if l.unread {
		return fmt.Errorf("invalid usage of UnreadLexeme")
	} else {
		l.unread = true
		return nil
	}
}

func Lex(r io.Reader) ([]Lexeme, error) {
	lexemeReader := NewLexemeReader(r)
	var lexemes []Lexeme
	for {
		if lexeme, err := lexemeReader.ReadLexeme(); err == nil {
			lexemes = append(lexemes, lexeme)
		} else if err == io.EOF {
			return lexemes, nil
		} else {
			return nil, err
		}
	}
}

func LexString(s string) ([]Lexeme, error) {
	return Lex(strings.NewReader(s))
}
