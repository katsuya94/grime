package grime

import (
	"bufio"
	"fmt"
	"io"
	"log"
	"strings"
	"unicode"
	"encoding/hex"
)

type Lexeme interface{}

type Identifier string
type Boolean bool
type Number string
type Character rune
type LeftParenthesis struct{}
type RightParenthesis struct{}

type Lexer struct {
	reader *bufio.Reader
}

func (l *Lexer) next() (rune, error) {
	r, _, err := l.reader.ReadRune()
	return r, err
}

func (l *Lexer) back() {
	l.reader.UnreadRune()
}

func (l *Lexer) snap(i int) {
	for ; i > 0; i-- {
		l.back()
	}
}

func (l *Lexer) readInterlexemeSpace() error {
	for {
		r, err := l.next()
		if err != nil {
			return err
		}
		switch r {
		case ' ':
		case '\n':
		default:
			l.back()
			return nil
		}
	}
}

func letter(r rune) bool {
	return (65 <= r && r < 91) ||
		(97 <= r && r < 123)
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
	return 48 <= r && r < 58
}

func hexDigit(r rune) bool {
	return digit(r) ||
		(65 <= r && r < 71) ||
		(97 <= r && r < 103)
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

func (l *Lexer) readIdentifier() (Lexeme, error) {
	var runes []rune
	if r, err := l.next(); initial(r) && err == nil {
		runes = append(runes, r)
	} else {
		l.back()
		return nil, err
	}
	for {
		if r, err := l.next(); subsequent(r) && err == nil {
			runes = append(runes, r)
		} else if (delimiter(r) && err == nil) || err == io.EOF {
			log.Printf("delimiter %#v", string(r))
			l.back()
			return Identifier(runes), err
		} else if err == nil {
			return nil, fmt.Errorf("unexpected rune: %#v", string(r))
		} else {
			return nil, err
		}
	}
}

func (l *Lexer) readBoolean() (Lexeme, error) {
	if r, err := l.next(); r != '#' {
		l.back()
		return nil, err
	}
	r, err := l.next()
	if err != nil {
		return nil, err
	}
	var lexeme Lexeme
	switch r {
	case 'f':
		lexeme = Boolean(false)
	case 'F':
		lexeme = Boolean(false)
	case 't':
		lexeme = Boolean(true)
	case 'T':
		lexeme = Boolean(true)
	default:
		l.back()
		l.back()
		return nil, nil
	}
    if r, err := l.next(); (delimiter(r) && err == nil) || err == io.EOF {
		log.Printf("delimiter %#v", string(r))
		l.back()
		return lexeme, err
	} else if err == nil {
		return nil, fmt.Errorf("expected delimiter; got: %#v", string(r))
	} else {
		return nil, err
	}
}

func (l *Lexer) readNumber() (Lexeme, error) {
	var runes []rune
	if r, err := l.next(); digit(r) && err == nil {
		runes = append(runes, r)
	} else {
		l.back()
		return nil, err
	}
	for {
		if r, err := l.next(); digit(r) && err == nil {
			runes = append(runes, r)
		} else if (delimiter(r) && err == nil) || err == io.EOF {
			log.Printf("delimiter %#v", string(r))
			l.back()
			return Number(runes), err
		} else if err == nil {
			return nil, fmt.Errorf("unexpected rune: %#v", string(r))
		} else {
			return nil, err
		}
	}
}

func (l *Lexer) readCharacter() (Lexeme, error) {
	if r, err := l.next(); r != '#' {
		l.back()
		return nil, err
	}
	if r, err := l.next(); r != '\\' {
		l.back()
		l.back()
		return nil, err
	}
	r, err := l.next()
	if err != nil {
		l.back()
		l.back()
		l.back()
		return nil, err
	}
	first := r
	if r, err := l.next(); (delimiter(r) && err == nil) || err == io.EOF {
		log.Printf("delimiter %#v", string(r))
		l.back()
		return Character(r), err
	} else if first == 'x' {
		var hexDigits []byte
		for {
			if r, err := l.next(); hexDigit(r) && err == nil {
				hexDigits = append(hexDigits, r)
			} else if (delimiter(r) && err == nil) || err == io.EOF {
				if len(hexDigits) % 2 == 1 {
					hexDigits = append([]byte{'0'}, hexDigits...)
				}
				var bytes []byte
				hex.Decode(bytes, hexDigits)
				var i
				for i, byte := bytes {
					if byte != 0 {
						break
					}
				}
				bytes = bytes[i:]
				if len(bytes) > 4 {
					return nil, fmt.Errorf("character out of range: %v bytes for maximum 4", len(bytes))
				}
				r = 0
				for _, byte := range bytes {
					r <<= 1
					r += rune(byte)
				}
				return Character(r), err
			} else if err == nil {
				return nil, fmt.Errorf("unexpected rune: %#v", string(r))
			} else {
				return nil, err
			}
		}
	} else {
		return nil, fmt.Errorf("TODO")
	}

}

func (l *Lexer) readLexeme() (Lexeme, error) {
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
	r, err := l.next()
	if err != nil {
		return nil, err
	}
	switch r {
	case '(':
		return LeftParenthesis{}, nil
	case ')':
		return RightParenthesis{}, nil
	default:
		return nil, fmt.Errorf("unexpected rune: %#v", string(r))
	}
}

func (l *Lexer) Lex() ([]Lexeme, error) {
	var lexemes []Lexeme
	for {
		err := l.readInterlexemeSpace()
		if err == io.EOF {
			return lexemes, nil
		} else if err != nil {
			return nil, err
		}
		log.Printf("reading lexeme...")
		lexeme, err := l.readLexeme()
		log.Printf("lexeme: %#v err: %v", lexeme, err)
		lexemes = append(lexemes, lexeme)
		if err == io.EOF {
			return lexemes, nil
		} else if err != nil {
			return nil, err
		}
	}
}

func NewLexer(r io.Reader) *Lexer {
	return &Lexer{bufio.NewReader(r)}
}

func Lex(r io.Reader) ([]Lexeme, error) {
	return NewLexer(r).Lex()
}

func LexString(s string) ([]Lexeme, error) {
	return Lex(strings.NewReader(s))
}
