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

type Number int

const (
	LeftParenthesis = iota
	RightParenthesis
)

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
	var (
		runes []rune
	)
	if r, err := l.next(); initial(r) && err == nil {
		log.Printf("valid initial %#v", string(r))
		runes = append(runes, r)
	} else {
		l.snap(len(runes) + 1)
		return nil, err
	}
	for {
		if r, err := l.next(); subsequent(r) && err == nil {
			log.Printf("valid subsequent %#v", string(r))
			runes = append(runes, r)
		} else if (delimiter(r) && err == nil) || err == io.EOF {
			log.Printf("delimiter %#v", string(r))
			l.back()
			return Identifier(runes), err
		} else {
			l.snap(len(runes) + 1)
			return nil, err
		}
	}
}

func (l *Lexer) readLexeme() (Lexeme, error) {
	if identifier, err := l.readIdentifier(); err != nil && err != io.EOF {
		return nil, err
	} else if identifier != nil {
		return identifier, err
	}
	r, err := l.next()
	if err != nil {
		return nil, err
	}
	switch r {
	case '(':
		return LeftParenthesis, nil
	case ')':
		return RightParenthesis, nil
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
