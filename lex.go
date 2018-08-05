package grime

import (
	"bufio"
	"fmt"
	"io"
	"log"
	"strings"
	"unicode"
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
	buffer []rune
	offset int
	eof    bool
}

func (l *Lexer) next() (rune, error) {
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
			log.Printf("EOF")
			l.eof = true
		} else {
			log.Printf("read %#v", string(r))
			l.buffer = append(l.buffer, r)
		}
	}
	l.offset += 1
	log.Printf("offset: %v buffer: %v eof: %v", l.offset, string(l.buffer), l.eof)
	return r, err
}

func (l *Lexer) delimit() {
	l.buffer = l.buffer[l.offset-1:]
	l.offset = 0
	log.Printf("offset: %v buffer: %v eof: %v", l.offset, string(l.buffer), l.eof)
}

func (l *Lexer) restore() {
	l.offset = 0
	log.Printf("offset: %v buffer: %v eof: %v", l.offset, string(l.buffer), l.eof)
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

func (l *Lexer) nextNonDelimiter() (rune, error) {
	if r, err := l.next(); (delimiter(r) && err == nil) || err == io.EOF {
		return 0, io.EOF
	} else {
		return r, err
	}
}

func (l *Lexer) nextExpectingNonDelimiter() (rune, error) {
	if r, err := l.nextNonDelimiter(); err == io.EOF {
		return 0, fmt.Errorf("unexpected delimiter %#v", string(r))
	} else {
		return r, err
	}
}

func (l *Lexer) nextExpectingDelimiter() error {
	if r, err := l.nextNonDelimiter(); err == io.EOF {
		return nil
	} else if err == nil {
		return fmt.Errorf("expected delimiter; got %#v", string(r))
	} else {
		return err
	}
}

func (l *Lexer) nextSatisfying(p func(rune) bool) (rune, error) {
	if r, err := l.next(); p(r) && err == nil {
		return r, nil
	} else if err == nil {
		return 0, io.EOF
	} else {
		return 0, err
	}
}

func (l *Lexer) nextExact(e rune) error {
	if r, err := l.next(); r == e && err == nil {
		return nil
	} else if err == nil {
		return io.EOF
	} else {
		return err
	}
}

func (l *Lexer) nextNonDelimiterExpecting(p func(rune) bool) (rune, error) {
	if r, err := l.nextNonDelimiter(); p(r) && err == nil {
		return r, nil
	} else if err == nil {
		return 0, fmt.Errorf("unexpected rune: %#v", string(r))
	} else {
		return 0, err
	}
}

func (l *Lexer) nextExactExpectingNonDelimiter(e rune) error {
	if r, err := l.nextNonDelimiter(); r == e && err == nil {
		return nil
	} else if err == nil {
		return io.EOF
	} else if err == io.EOF {
		return fmt.Errorf("unexpected delimiter %#v", string(r))
	} else {
		return err
	}
}

func (l *Lexer) readInterlexemeSpace() error {
	for {
		if r, err := l.next(); (!whitespace(r) && err == nil) || err == io.EOF {
			l.delimit()
			return err
		} else if err != nil {
			return err
		}
	}
}

func (l *Lexer) readIdentifier() (Lexeme, error) {
	var runes []rune
	if r, err := l.nextSatisfying(initial); err == nil {
		runes = append(runes, r)
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

func (l *Lexer) readBoolean() (Lexeme, error) {
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

func (l *Lexer) readNumber() (Lexeme, error) {
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
			return Number(runes), err
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

func (l *Lexer) readCharacter() (Lexeme, error) {
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
	if _, err := l.nextNonDelimiter(); err == io.EOF {
		l.delimit()
		return Character(first), err
	} else if err == nil && first == 'x' {
		var hexDigits []rune
		l.delimit()
		for {
			if r, err := l.nextNonDelimiterExpecting(hexDigit); err == nil {
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
				if len(hexDigits) - i > 8 {
					return nil, fmt.Errorf(`character out of range: \#x%v`, string(hexDigits))
				}
				log.Print(string(hexDigits))
				hexDigits = hexDigits[i:]
				log.Print(string(hexDigits))
				r = 0
				for _, hexDigit := range hexDigits {
					r <<= 4
					r += hexValue(hexDigit)
				}
				return Character(r), err
			} else if err != nil {
				return nil, err
			}
		}
	} else if err == nil {
		return nil, fmt.Errorf("TODO")
	} else {
		return nil, err
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
	if character, err := l.readCharacter(); err != nil && err != io.EOF {
		return nil, err
	} else if character != nil {
		return character, err
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
	return &Lexer{bufio.NewReader(r), nil, 0, false}
}

func Lex(r io.Reader) ([]Lexeme, error) {
	return NewLexer(r).Lex()
}

func LexString(s string) ([]Lexeme, error) {
	return Lex(strings.NewReader(s))
}
