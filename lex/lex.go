package grime

import (
	"bufio"
	"fmt"
	"io"
	"strings"
	"unicode"
)

type Lexeme interface{}

type Identifier string
type Boolean bool
type Number string
type Character rune
type String string
type LeftParenthesis struct{}
type RightParenthesis struct{}
type LeftBracket struct{}
type RightBracket struct{}
type Quote struct{}
type QuasiQuote struct{}
type Unquote struct{}
type UnquoteSplice struct{}
type Pair struct{}

type LexemeReader struct {
	reader *bufio.Reader
	buffer []rune
	offset int
	eof    bool
}

func (l *LexemeReader) next() (rune, error) {
	var (
		r   rune
		err error
	)
	if l.offset < len(l.buffer) {
		r = l.buffer[l.offset]
	} else if l.eof {
		err = io.EOF
	} else {
		r, _, err = l.reader.ReadRune()
		if err == io.EOF {
			l.eof = true
		} else {
			l.buffer = append(l.buffer, r)
		}
	}
	l.offset += 1
	return r, err
}

func (l *LexemeReader) delimit() {
	l.buffer = l.buffer[l.offset-1:]
	l.offset = 0
}

func (l *LexemeReader) restore() {
	l.offset = 0
}

func letter(r rune) bool {
	return ('a' <= r && r <= 'z') ||
		('A' <= r && r <= 'Z')
}

func constituent(r rune) bool {
	return letter(r) || (r > 127 &&
		unicode.In(r,
			unicode.Lu,
			unicode.Ll,
			unicode.Lt,
			unicode.Lm,
			unicode.Lo,
			unicode.Mn,
			unicode.Nl,
			unicode.No,
			unicode.Pd,
			unicode.Pc,
			unicode.Po,
			unicode.Sc,
			unicode.Sm,
			unicode.Sk,
			unicode.So,
			unicode.Co))
}

func specialInitial(r rune) bool {
	switch r {
	case '!':
		return true
	case '$':
		return true
	case '%':
		return true
	case '&':
		return true
	case '*':
		return true
	case '/':
		return true
	case ':':
		return true
	case '<':
		return true
	case '=':
		return true
	case '>':
		return true
	case '?':
		return true
	case '^':
		return true
	case '_':
		return true
	case '~':
		return true
	default:
		return false
	}
}

func initial(r rune) bool {
	return constituent(r) ||
		specialInitial(r) // TODO inline hex escape
}

func whitespace(r rune) bool {
	switch r {
	case '\x09':
		return true
	case '\x0a':
		return true
	case '\x0b':
		return true
	case '\x0c':
		return true
	case '\x0d':
		return true
	case '\x85':
		return true
	default:
		return unicode.In(r,
			unicode.Zs,
			unicode.Zl,
			unicode.Zp)
	}
}

func delimiter(r rune) bool {
	switch r {
	case '(':
		return true
	case ')':
		return true
	case '[':
		return true
	case ']':
		return true
	case '"':
		return true
	case ';':
		return true
	case '#':
		return true
	default:
		return whitespace(r)
	}
}

func digit(r rune) bool {
	return '0' <= r && r <= '9'
}

func hexDigit(r rune) bool {
	return digit(r) ||
		('a' <= r && r <= 'f') ||
		('A' <= r && r <= 'F')
}

func specialSubsequent(r rune) bool {
	switch r {
	case '+':
		return true
	case '-':
		return true
	case '.':
		return true
	case '@':
		return true
	default:
		return false
	}
}

func subsequent(r rune) bool {
	return initial(r) ||
		digit(r) ||
		unicode.In(r,
			unicode.Nd,
			unicode.Mc,
			unicode.Me) ||
		specialSubsequent(r)
}

func intralineWhitespace(r rune) bool {
	return r == '\x09' || unicode.Is(unicode.Zs, r)
}

func (l *LexemeReader) nextNonDelimiter() (rune, error) {
	if r, err := l.next(); (delimiter(r) && err == nil) || err == io.EOF {
		return 0, io.EOF
	} else {
		return r, err
	}
}

func (l *LexemeReader) nextExpectingNonDelimiter() (rune, error) {
	if r, err := l.nextNonDelimiter(); err == io.EOF {
		return 0, fmt.Errorf("unexpected delimiter %#v", string(r))
	} else {
		return r, err
	}
}

func (l *LexemeReader) nextExpectingDelimiter() error {
	if r, err := l.nextNonDelimiter(); err == io.EOF {
		return nil
	} else if err == nil {
		return fmt.Errorf("expected delimiter; got %#v", string(r))
	} else {
		return err
	}
}

func (l *LexemeReader) nextSatisfying(p func(rune) bool) (rune, error) {
	if r, err := l.next(); p(r) && err == nil {
		return r, nil
	} else if err == nil {
		return 0, io.EOF
	} else {
		return 0, err
	}
}

func (l *LexemeReader) nextExact(e rune) error {
	if r, err := l.next(); r == e && err == nil {
		return nil
	} else if err == nil {
		return io.EOF
	} else {
		return err
	}
}

func (l *LexemeReader) nextNonDelimiterExpecting(p func(rune) bool) (rune, error) {
	if r, err := l.nextNonDelimiter(); p(r) && err == nil {
		return r, nil
	} else if err == nil {
		return 0, fmt.Errorf("unexpected rune: %#v", string(r))
	} else {
		return 0, err
	}
}

func (l *LexemeReader) nextExactExpectingNonDelimiter(e rune) error {
	if r, err := l.nextNonDelimiter(); r == e && err == nil {
		return nil
	} else if err == nil {
		return io.EOF
	} else if err == io.EOF {
		return fmt.Errorf("unexpected delimiter")
	} else {
		return err
	}
}

func (l *LexemeReader) nextExpectingExact(e rune) error {
	if r, err := l.next(); r == e && err == nil {
		return nil
	} else if err == nil {
		return fmt.Errorf("expected %#v; got %#v", string(e), string(r))
	} else if err == io.EOF {
		return fmt.Errorf("unexpected EOF")
	} else {
		return err
	}
}

func (l *LexemeReader) nextExpecting(p func(rune) bool) (rune, error) {
	if r, err := l.next(); p(r) && err == nil {
		return r, nil
	} else if err == nil {
		return 0, fmt.Errorf("unexpected rune: %#v", string(r))
	} else if err == io.EOF {
		return 0, fmt.Errorf("unexpected EOF")
	} else {
		return 0, err
	}
}

func (l *LexemeReader) readInterlexemeSpace() error {
	for {
		if r, err := l.next(); (!whitespace(r) && err == nil) || err == io.EOF {
			l.delimit()
			return err
		} else if err != nil {
			return err
		}
	}
}

func (l *LexemeReader) readIdentifier() (Lexeme, error) {
	var runes []rune
	if r, err := l.nextNonDelimiter(); initial(r) && err == nil {
		runes = append(runes, r)
	} else if err == nil {
		switch r {
		case '+':
			if _, err := l.nextNonDelimiter(); err == io.EOF {
				l.delimit()
				return Identifier("+"), nil
			} else if err == nil {
				l.delimit()
				return nil, nil
			} else {
				return nil, err
			}
		case '-':
			if r, err := l.nextNonDelimiter(); err == io.EOF {
				l.delimit()
				return Identifier("-"), nil
			} else if r == '>' && err == nil {
				runes = []rune{'-', '>'}
				for {
					if r, err := l.nextNonDelimiterExpecting(subsequent); err == nil {
						runes = append(runes, r)
					} else if err == io.EOF {
						l.delimit()
						return Identifier(runes), nil
					} else {
						return nil, err
					}
				}
			} else if err == nil {
				l.delimit()
				return nil, nil
			} else {
				return nil, err
			}
		case '.':
			if r, err := l.nextNonDelimiter(); err == io.EOF {
				l.restore()
				return nil, nil
			} else if r != '.' && err == nil {
				return nil, fmt.Errorf("unexpected rune: %#v", string(r))
			} else if err != nil {
				return nil, err
			}
			if err := l.nextExpectingExact('.'); err != nil {
				return nil, err
			}
			if err := l.nextExpectingDelimiter(); err == nil {
				l.delimit()
				return Identifier("..."), nil
			} else {
				return nil, err
			}
		default:
			l.delimit()
			return nil, nil
		}
	} else if err == io.EOF {
		l.delimit()
		return nil, nil
	} else {
		return nil, err
	}
	for {
		if r, err := l.nextNonDelimiterExpecting(subsequent); err == nil {
			runes = append(runes, r)
		} else if err == io.EOF {
			l.delimit()
			return Identifier(runes), nil
		} else {
			return nil, err
		}
	}
}

func (l *LexemeReader) readBoolean() (Lexeme, error) {
	if err := l.nextExact('#'); err == io.EOF {
		l.delimit()
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
		l.restore()
		return nil, nil
	}
	if err := l.nextExpectingDelimiter(); err == nil {
		l.delimit()
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
		l.delimit()
		return nil, nil
	} else {
		return nil, err
	}
	for {
		if r, err := l.nextNonDelimiterExpecting(digit); err == nil {
			runes = append(runes, r)
		} else if err == io.EOF {
			l.delimit()
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
			l.delimit()
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
		l.delimit()
		return nil, nil
	} else if err != nil {
		return nil, err
	}
	if err := l.nextExactExpectingNonDelimiter('\\'); err == io.EOF {
		l.delimit()
		return nil, nil
	} else if err != nil {
		return nil, err
	}
	first, err := l.nextExpectingNonDelimiter()
	if err != nil {
		return nil, err
	}
	if r, err := l.nextNonDelimiter(); err == io.EOF {
		l.delimit()
		return Character(first), nil
	} else if err == nil && first == 'x' {
		l.delimit()
		r, err := l.readHex()
		if err != nil {
			return nil, err
		}
		if err := l.nextExpectingDelimiter(); err != nil {
			return nil, err
		}
		l.delimit()
		return Character(r), err
	} else if err == nil {
		runes := []rune{first, r}
		for {
			if r, err := l.nextNonDelimiter(); err == io.EOF {
				l.delimit()
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
			} else if err == nil {
				runes = append(runes, r)
			} else {
				return nil, err
			}
		}
	} else {
		return nil, err
	}

}

func (l *LexemeReader) readLineEnding() (bool, error) {
	r, err := l.next()
	if err == io.EOF {
		return false, fmt.Errorf("unexpected EOF")
	} else if err != nil {
		return false, err
	}
	switch r {
	case '\x0a':
		return true, nil
	case '\x0d':
		r, err := l.next()
		if err == io.EOF {
			l.delimit()
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
			l.delimit()
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
		l.delimit()
		return nil, nil
	} else if err != nil {
		return nil, err
	}
	var runes []rune
	for {
		if r, err := l.next(); r == '"' && err == nil {
			if _, err := l.next(); err != nil && err != io.EOF {
				return nil, err
			}
			l.delimit()
			return String(string(runes)), nil
		} else if r == '\\' && err == nil {
			r, err := l.next()
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
			l.delimit()
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

func (l *LexemeReader) readLexeme() (Lexeme, error) {
	if identifier, err := l.readIdentifier(); err != nil && err != io.EOF {
		return nil, err
	} else if identifier != nil {
		return identifier, err
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
	r, err := l.next()
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
			return UnquoteSplice{}, nil
		} else {
			l.delimit()
			return Unquote{}, nil
		}
	case '.':
		return Pair{}, nil
	default:
		return nil, fmt.Errorf("unexpected rune: %#v", string(r))
	}
}

func (l *LexemeReader) ReadLexeme() (Lexeme, error) {
	if err := l.readInterlexemeSpace(); err != nil {
		return nil, err
	}
	lexeme, err := l.readLexeme()
	if err == io.EOF {
		return nil, fmt.Errorf("unexpected EOF")
	} else if err != nil {
		return nil, err
	}
	return lexeme, nil
}

func NewLexemeReader(r io.Reader) *LexemeReader {
	return &LexemeReader{bufio.NewReader(r), nil, 0, false}
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
