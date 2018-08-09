package lex

import (
	"reflect"
	"testing"
)

func TestLex(t *testing.T) {
	tests := []struct {
		name    string
		source  string
		lexemes []Lexeme
		error   string
	}{
		{
			"identifier",
			"id",
			[]Lexeme{Identifier("id")},
			"",
		},
		{
			"identifier with whitespace",
			"  id ",
			[]Lexeme{Identifier("id")},
			"",
		},
		{
			"two identifiers",
			"id name",
			[]Lexeme{Identifier("id"), Identifier("name")},
			"",
		},
		{
			"parentheses",
			"(id)",
			[]Lexeme{LeftParenthesis{}, Identifier("id"), RightParenthesis{}},
			"",
		},
		{
			"parentheses with whitespace",
			"(  id )",
			[]Lexeme{LeftParenthesis{}, Identifier("id"), RightParenthesis{}},
			"",
		},
		{
			"parentheses with multiple members",
			"(id name)",
			[]Lexeme{LeftParenthesis{}, Identifier("id"), Identifier("name"), RightParenthesis{}},
			"",
		},
		{
			"empty parentheses",
			"()",
			[]Lexeme{LeftParenthesis{}, RightParenthesis{}},
			"",
		},
		{
			"false",
			"#f",
			[]Lexeme{Boolean(false)},
			"",
		},
		{
			"false capital",
			"#F",
			[]Lexeme{Boolean(false)},
			"",
		},
		{
			"true",
			"#t",
			[]Lexeme{Boolean(true)},
			"",
		},
		{
			"true capital",
			"#T",
			[]Lexeme{Boolean(true)},
			"",
		},
		{
			"boolean without delimiter",
			"#fn",
			nil,
			"expected delimiter",
		},
		{
			"number",
			"123",
			[]Lexeme{Number("123")},
			"",
		},
		{
			"character x",
			`#\x`,
			[]Lexeme{Character('x')},
			"",
		},
		{
			"character hex",
			`#\x6e`,
			[]Lexeme{Character('n')},
			"",
		},
		{
			"character hex capital",
			`#\x6E`,
			[]Lexeme{Character('n')},
			"",
		},
		{
			"character hex with zeros",
			`#\x06e`,
			[]Lexeme{Character('n')},
			"",
		},
		{
			"character hex with many zeros",
			`#\x000000006e`,
			[]Lexeme{Character('n')},
			"",
		},
		{
			"character hex out of range",
			`#\x123456789abcdef`,
			nil,
			`character out of range: \#x123456789abcdef`,
		},
		{
			"character named",
			`#\nul`,
			[]Lexeme{Character('\x00')},
			"",
		},
		{
			"character unrecognized name",
			`#\foo`,
			nil,
			`unrecognized character: #\foo`,
		},
		{
			"string",
			`"name"`,
			[]Lexeme{String("name")},
			"",
		},
		{
			`string escape "`,
			`"\""`,
			[]Lexeme{String(`"`)},
			"",
		},
		{
			`string escape \`,
			`"\\"`,
			[]Lexeme{String(`\`)},
			"",
		},
		{
			"string escape hex",
			`"\x6e;"`,
			[]Lexeme{String("n")},
			"",
		},
		{
			"string escape hex unterminated",
			`"\x6e"`,
			nil,
			"unexpected rune",
		},
		{
			"string escape hex empty",
			`"\x;"`,
			nil,
			"unexpected delimiter",
		},
		{
			"string escape hex non hex digit",
			`"\xe;"`,
			nil,
			"unexpected rune",
		},
		{
			"string line ending",
			"\"\x0a\"",
			[]Lexeme{String("\x0a")},
			"",
		},
		{
			"string carriage return line ending",
			"\"\x0d\x0a\"",
			[]Lexeme{String("\x0a")},
			"",
		},
		{
			"string escape intraline whitespace line ending intraline whitespace",
			"\"\\ \x0a \"",
			[]Lexeme{String("")},
			"",
		},
		{
			"string escape intraline whitespace carriage return line ending intraline whitespace",
			"\"\\ \x0d\x0a \"",
			[]Lexeme{String("")},
			"",
		},
		{
			"unquote",
			"`(,a)",
			[]Lexeme{QuasiQuote{}, LeftParenthesis{}, Unquote{}, Identifier("a"), RightParenthesis{}},
			"",
		},
		{
			"unquote splicing",
			"`(,@a)",
			[]Lexeme{QuasiQuote{}, LeftParenthesis{}, UnquoteSplicing{}, Identifier("a"), RightParenthesis{}},
			"",
		},
		{
			"pair",
			"(a . b)",
			[]Lexeme{LeftParenthesis{}, Identifier("a"), Dot{}, Identifier("b"), RightParenthesis{}},
			"",
		},
		{
			"identifier +",
			"+",
			[]Lexeme{Identifier("+")},
			"",
		},
		{
			"identifier -",
			"-",
			[]Lexeme{Identifier("-")},
			"",
		},
		{
			"identifier ...",
			"...",
			[]Lexeme{Identifier("...")},
			"",
		},
		{
			"identifier ->",
			"->",
			[]Lexeme{Identifier("->")},
			"",
		},
		{
			"identifier -> with trailing",
			"->id",
			[]Lexeme{Identifier("->id")},
			"",
		},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			lexemes, err := LexString(test.source)
			if test.error == "" && err != nil {
				t.Fatal(err)
			} else if test.error != "" && err != nil && err.Error() != test.error {
				t.Errorf("\nexpected error: %v\n     got error: %v\n", test.error, err.Error())
			}
			if test.lexemes != nil && !reflect.DeepEqual(lexemes, test.lexemes) {
				t.Errorf("\nexpected: %#v\n     got: %#v", test.lexemes, lexemes)
			}
		})
	}
}
