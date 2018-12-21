package read

import (
	"fmt"
	"io"
	"strings"

	"github.com/katsuya94/grime/common"
)

type LexemeReader struct {
	file   string
	reader *CheckpointedRuneReader
	next   Lexeme
	unread bool
}

func NewLexemeReader(file string, r io.Reader) *LexemeReader {
	return &LexemeReader{
		file,
		NewCheckpointedRuneReader(r),
		nil,
		false,
	}
}

func (l *LexemeReader) ReadLexeme() (Lexeme, common.SourceLocation, error) {
	if l.unread {
		l.unread = false
		return l.next, common.SourceLocation{}, nil
	}
	if err := l.readInterlexemeSpace(); err != nil {
		return nil, common.SourceLocation{}, err
	}
	l.reader.Checkpoint()
	sourceLocation := l.sourceLocation()
	lexeme, err := l.expectLexeme()
	if err != nil {
		return nil, common.SourceLocation{}, err
	}
	sourceLocation.Length = l.reader.Offset() - sourceLocation.Offset
	l.next = lexeme
	return lexeme, sourceLocation, nil
}

func (l *LexemeReader) sourceLocation() common.SourceLocation {
	return common.SourceLocation{
		File:   l.file,
		Line:   l.reader.Line(),
		Column: l.reader.Column(),
		Offset: l.reader.Offset(),
	}
}

func (l *LexemeReader) errorf(format string, a ...interface{}) ReadError {
	return ReadError{
		l.sourceLocation(),
		fmt.Sprintf(format, a...),
	}
}

func (l *LexemeReader) runeErrorf(format string, a ...interface{}) ReadError {
	l.reader.UnreadRune()
	return l.errorf(format, a...)
}

func (l *LexemeReader) lexemeErrorf(format string, a ...interface{}) ReadError {
	offset := l.reader.Offset()
	l.reader.Return()
	err := l.errorf(format, a...)
	err.Length = offset - err.Offset
	return err
}

func (l *LexemeReader) readInterlexemeSpace() error {
	for {
		r, err := l.reader.ReadRune()
		if err != nil {
			return err
		} else if whitespace(r) {
			continue
		} else if r == ';' {
			for {
				r, err := l.reader.ReadRune()
				if err != nil {
					return err
				} else if paragraphSeparator(r) {
					break
				}
				l.reader.UnreadRune()
				ok, err := l.readLineEnding()
				if err != nil {
					return err
				} else if ok {
					break
				}
				l.reader.ReadRune()
			}
			continue
		} else if r == '#' {
			r, err := l.reader.ReadRune()
			if err != nil {
				return err
			} else if r == ';' {
				err := l.readInterlexemeSpace()
				if err != nil {
					return err
				}
				_, _, err = (&DatumReader{l}).expectDatum()
				if err != nil {
					return err
				}
				continue
			}
			l.reader.UnreadRune()
		}
		l.reader.UnreadRune()
		ok, err := l.readNestedComment()
		if err != nil {
			return err
		} else if ok {
			continue
		}
		return nil
	}
}

func (l *LexemeReader) readNestedComment() (bool, error) {
	r, ok, err := l.readNonEOF()
	if err != nil {
		return false, err
	} else if !ok {
		return false, nil
	} else if r != '#' {
		l.reader.UnreadRune()
		return false, nil
	}
	r, ok, err = l.readNonEOF()
	if err != nil {
		return false, err
	} else if !ok {
		l.reader.UnreadRune()
		return false, nil
	} else if r != '|' {
		l.reader.UnreadRune()
		l.reader.UnreadRune()
		return false, nil
	}
	for {
		r, err = l.expectNonEOF()
		if err != nil {
			return false, err
		} else if r == '|' {
			r, err = l.expectNonEOF()
			if err != nil {
				return false, err
			} else if r == '#' {
				return true, nil
			}
			l.reader.UnreadRune()
			continue
		}
		l.reader.UnreadRune()
		ok, err := l.readNestedComment()
		if err != nil {
			return false, err
		} else if !ok {
			l.reader.ReadRune()
		}
		continue
	}
}

func (l *LexemeReader) expectLexeme() (Lexeme, error) {
	readFns := []func() (Lexeme, bool, error){
		l.readIdentifier,
		l.readBoolean,
		l.readNumber,
		l.readCharacter,
		l.readString,
	}
	for _, readFn := range readFns {
		if lexeme, ok, err := readFn(); err != nil {
			return nil, err
		} else if ok {
			return lexeme, nil
		}
	}
	r, err := l.expectNonEOF()
	if err != nil {
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
		return Quasiquote{}, nil
	case ',':
		if r, ok, err := l.readNonEOF(); err != nil {
			return nil, err
		} else if ok && r == '@' {
			return UnquoteSplicing{}, nil
		} else {
			l.reader.UnreadRune()
			return Unquote{}, nil
		}
	case '#':
		r, err := l.expectNonEOF()
		if err != nil {
			return nil, err
		}
		switch r {
		case '\'':
			return Syntax{}, nil
		case '`':
			return Quasisyntax{}, nil
		case ',':
			if r, ok, err := l.readNonEOF(); err != nil {
				return nil, err
			} else if ok && r == '@' {
				return UnsyntaxSplicing{}, nil
			} else {
				l.reader.UnreadRune()
				return Unsyntax{}, nil
			}
		default:
			return nil, l.runeErrorf("unexpected rune")
		}
	case '.':
		return Dot{}, nil
	default:
		return nil, l.runeErrorf("unexpected rune")
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
		if err := l.expectDelimiter(); err != nil {
			return nil, false, err
		}
		return Identifier("+"), true, nil
	case '-':
		if r, ok, err := l.readNonDelimiter(); err != nil {
			return nil, false, err
		} else if !ok {
			return Identifier("-"), true, nil
		} else if r != '>' {
			return nil, false, l.runeErrorf("expected delimiter or >")
		}
		return l.readSubsequent([]rune{'-', '>'})
	case '.':
		if r, ok, err := l.readNonDelimiter(); err != nil {
			return nil, false, err
		} else if !ok {
			l.reader.Return()
			return nil, false, nil
		} else if r != '.' {
			return nil, false, l.runeErrorf("expected .")
		}
		if r, err := l.expectNonDelimiter(); err != nil {
			return nil, false, err
		} else if r != '.' {
			return nil, false, l.runeErrorf("expected .")
		}
		if err := l.expectDelimiter(); err != nil {
			return nil, false, err
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
			break
		}
	}
	return Identifier(runes), true, nil
}

func (l *LexemeReader) readBoolean() (Lexeme, bool, error) {
	if r, err := l.expectNonEOF(); err != nil {
		return nil, false, err
	} else if r != '#' {
		l.reader.Return()
		return nil, false, nil
	}
	r, err := l.expectNonDelimiter()
	if err != nil {
		return nil, false, err
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
		l.reader.Return()
		return nil, false, nil
	}
	if err := l.expectDelimiter(); err != nil {
		return nil, false, err
	}
	return lexeme, true, nil
}

func (l *LexemeReader) readNumber() (Lexeme, bool, error) {
	r, ok, err := l.readNonDelimiter()
	if err != nil {
		return nil, false, err
	} else if !ok {
		return nil, false, nil
	} else if !digit(r) {
		return nil, false, nil
	}
	runes := []rune{r}
	for {
		if r, ok, err := l.readNonDelimiter(); err != nil {
			return nil, false, err
		} else if !ok {
			return Number(runes), true, nil
		} else if !digit(r) {
			return nil, false, l.runeErrorf("unexpected rune in number")
		} else {
			runes = append(runes, r)
		}
	}
}

func (l *LexemeReader) readCharacter() (Lexeme, bool, error) {
	if r, err := l.expectNonEOF(); err != nil {
		return nil, false, err
	} else if r != '#' {
		l.reader.Return()
		return nil, false, nil
	}
	if r, err := l.expectNonDelimiter(); err != nil {
		return nil, false, err
	} else if r != '\\' {
		l.reader.Return()
		return nil, false, nil
	}
	first, err := l.expectNonDelimiter()
	if err != nil {
		return nil, false, err
	}
	if r, ok, err := l.readNonDelimiter(); err != nil {
		return nil, false, err
	} else if !ok {
		return Character(first), true, nil
	} else if first == 'x' {
		l.reader.UnreadRune()
		r, err := l.expectHex()
		if err != nil {
			return nil, false, err
		}
		if err := l.expectDelimiter(); err != nil {
			return nil, false, err
		}
		return Character(r), true, err
	} else {
		runes := []rune{first, r}
		for {
			if r, ok, err := l.readNonDelimiter(); err != nil {
				return nil, false, err
			} else if ok {
				runes = append(runes, r)
			} else {
				break
			}
		}
		switch string(runes) {
		case "nul":
			return Character('\x00'), true, nil
		case "alarm":
			return Character('\x07'), true, nil
		case "backspace":
			return Character('\x08'), true, nil
		case "tab":
			return Character('\x09'), true, nil
		case "linefeed":
			return Character('\x0a'), true, nil
		case "newline":
			return Character('\x0a'), true, nil
		case "vtab":
			return Character('\x0b'), true, nil
		case "page":
			return Character('\x0c'), true, nil
		case "return":
			return Character('\x0d'), true, nil
		case "esc":
			return Character('\x1b'), true, nil
		case "space":
			return Character('\x20'), true, nil
		case "delete":
			return Character('\x7f'), true, nil
		default:
			return nil, false, l.lexemeErrorf(`unrecognized character: #\%v`, string(runes))
		}
	}
}

func (l *LexemeReader) readString() (Lexeme, bool, error) {
	if r, err := l.expectNonEOF(); err != nil {
		return nil, false, err
	} else if r != '"' {
		l.reader.Return()
		return nil, false, nil
	}
	var runes []rune
	for {
		if r, err := l.expectNonEOF(); err != nil {
			return nil, false, err
		} else if r == '"' {
			return String(runes), true, nil
		} else if r == '\\' {
			if r, ok, err := l.readStringEscape(); err != nil {
				return nil, false, err
			} else if ok {
				runes = append(runes, r)
			}
		} else {
			l.reader.UnreadRune()
			if ok, err := l.readLineEnding(); err != nil {
				return nil, false, err
			} else if ok {
				runes = append(runes, '\x0a')
			} else {
				runes = append(runes, r)
				l.reader.ReadRune()
			}
		}
	}
}

func (l *LexemeReader) readStringEscape() (rune, bool, error) {
	r, err := l.expectNonEOF()
	if err != nil {
		return 0, false, err
	}
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
		r, err = l.expectHex()
		if err != nil {
			return 0, false, err
		}
		if r, err := l.expectNonEOF(); err != nil {
			return 0, false, err
		} else if r != ';' {
			return 0, false, l.runeErrorf("expected ;")
		}
	default:
		if intralineWhitespace(r) {
			if ok, err := l.readLineEnding(); err != nil {
				return 0, false, err
			} else if !ok {
				return 0, false, l.runeErrorf("expected line ending")
			}
			if r, err := l.expectNonEOF(); err != nil {
				return 0, false, err
			} else if !intralineWhitespace(r) {
				return 0, false, l.runeErrorf("expected intraline whitespace")
			}
			return 0, false, nil
		} else {
			return 0, false, l.lexemeErrorf(`unrecognized escape sequence: \%v`, r)
		}
	}
	return r, true, nil
}

func (l *LexemeReader) readLineEnding() (bool, error) {
	r, err := l.expectNonEOF()
	if err != nil {
		return false, err
	}
	switch r {
	case '\x0a':
		return true, nil
	case '\x0d':
		r, ok, err := l.readNonEOF()
		if err != nil {
			return false, err
		} else if !ok {
			return true, nil
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
		l.reader.UnreadRune()
		return false, nil
	}
}

func (l *LexemeReader) expectHex() (rune, error) {
	r, err := l.expectNonDelimiter()
	if err != nil {
		return 0, err
	} else if !hexDigit(r) {
		return 0, l.runeErrorf("expected hex digit")
	}
	hexDigits := []rune{r}
	for {
		if r, ok, err := l.readNonDelimiter(); err != nil {
			return 0, err
		} else if !ok {
			break
		} else if hexDigit(r) {
			hexDigits = append(hexDigits, r)
		} else {
			return 0, err
		}
	}
	r = 0
	for _, hexDigit := range hexDigits {
		r <<= 4
		r += hexValue(hexDigit)
	}
	if r < 0 || (0xd800 <= r && r <= 0xdfff) || 0x10ffff < r {
		offset := l.reader.Offset()
		for range hexDigits {
			l.reader.UnreadRune()
		}
		err := l.errorf("invalid hex scalar value: %v", string(hexDigits))
		err.Length = offset - l.reader.Offset()
		return 0, err
	}
	return r, nil
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

func (l *LexemeReader) expectDelimiter() error {
	if _, ok, err := l.readNonDelimiter(); err != nil {
		return err
	} else if ok {
		return l.runeErrorf("expected delimiter")
	} else {
		return nil
	}
}

func (l *LexemeReader) expectNonDelimiter() (rune, error) {
	if r, ok, err := l.readNonDelimiter(); err != nil {
		return 0, err
	} else if ok {
		return r, nil
	} else {
		return 0, l.errorf("unexpected delimiter")
	}
}

func (l *LexemeReader) readNonDelimiter() (rune, bool, error) {
	if r, ok, err := l.readNonEOF(); err != nil {
		return 0, false, err
	} else if !ok {
		return 0, false, nil
	} else if delimiter(r) {
		l.reader.UnreadRune()
		return 0, false, nil
	} else {
		return r, true, nil
	}
}

func (l *LexemeReader) expectNonEOF() (rune, error) {
	if r, ok, err := l.readNonEOF(); err != nil {
		return 0, err
	} else if ok {
		return r, nil
	} else {
		return 0, NewUnexpectedEOFError(l.sourceLocation())
	}
}

func (l *LexemeReader) readNonEOF() (rune, bool, error) {
	if r, err := l.reader.ReadRune(); err == io.EOF {
		l.reader.UnreadRune()
		return 0, false, nil
	} else if err != nil {
		return 0, false, err
	} else {
		return r, true, nil
	}
}

func (l *LexemeReader) UnreadLexeme() {
	if l.unread {
		panic("invalid usage of UnreadLexeme")
	}
	l.unread = true
}

func Lex(file string, r io.Reader) ([]Lexeme, []common.SourceLocation, error) {
	lexemeReader := NewLexemeReader(file, r)
	var (
		lexemes         []Lexeme
		sourceLocations []common.SourceLocation
	)
	for {
		lexeme, sourceLocation, err := lexemeReader.ReadLexeme()
		if err == io.EOF {
			break
		} else if err != nil {
			return nil, nil, err
		}
		lexemes = append(lexemes, lexeme)
		sourceLocations = append(sourceLocations, sourceLocation)
	}
	return lexemes, sourceLocations, nil
}

func LexString(s string) ([]Lexeme, []common.SourceLocation, error) {
	return Lex("string", strings.NewReader(s))
}
