package grime

import (
	"bufio"
	"io"
	"strings"
	"fmt"
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
	for i; i > 0; i-- {
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
	return (65 <= r && r < 91 ) ||
		(97 <= r && r < 123)
}

func constituent(r rune) bool {
	return letter(r) ||
		r > 127 // TODO unicode categories
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
		return false // TODO unicode categories
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

func subsequent(r rune) bool {

}

func (l *Lexer) readIdentifier() (Lexeme, error) {
	var (
		runes []rune
	)
	r, err := l.next();
	runes = append(runes, r)
	if err != nil {
		return nil, err
	} else if !initial(r) {
		l.snap(len(runes))
		return nil, nil
	}
	for {
		r, err := l.next();
		runes = append(runes, r)
		if err != nil {
			return nil, err
		} else if delimiter(r) {
			return Identifier(runes), nil
		} else if !subsequent(r) {
			l.snap(len(runes))
			return nil, nil
		}
	}
}

func (l *Lexer) readLexeme() (Lexeme, error) {
	if identifier, err := l.readIdentifier(); err != nil {
		return nil, err
	} else if identifier != "" {
		return identifier, nil
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
		return nil, fmt.Errorf("unexpected rune: %v", string(r))
	}
}

func (l *Lexer) Lex() ([]Lexeme, error) {
	var lexemes []Lexeme
	for {
		err := l.readInterlexemeSpace()
		if err == io.EOF {
			return lexemes, nil
		} else if err != nil {
			return nil, nil
		}
		lexeme, err := l.readLexeme()
		lexemes = append(lexemes, lexeme)
		if err == io.EOF {
			return lexemes, nil
		} else if err != nil {
			return nil, nil
		}
	}
}

func NewLexer(r io.Reader) *Lexer {
	return &Lexer{bufio.NewReader(r) }
}

func Lex(r io.Reader) ([]Lexeme, error) {
	return NewLexer(r).Lex()
}

func LexString(s string) ([]Lexeme, error) {
	return Lex(strings.NewReader(s))
}
