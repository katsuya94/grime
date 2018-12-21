package read

import (
	"fmt"
	"reflect"
	"testing"
)

func sl(formatted string) SourceLocation {
	sourceLocation := SourceLocation{File: "string"}
	fmt.Sscanf(formatted, "%d:%d:%d:%d", &sourceLocation.Line, &sourceLocation.Column, &sourceLocation.Offset, &sourceLocation.Length)
	return sourceLocation
}

func TestLex(t *testing.T) {
	tests := []struct {
		name            string
		source          string
		lexemes         []Lexeme
		sourceLocations []SourceLocation
		error           string
	}{
		{
			"line comment",
			"id ; comment\nname",
			[]Lexeme{Identifier("id"), Identifier("name")},
			[]SourceLocation{sl("0:0:0:2"), sl("1:0:13:4")},
			"",
		},
		{
			"line comment with whitespace after",
			"id ; comment\n name",
			[]Lexeme{Identifier("id"), Identifier("name")},
			[]SourceLocation{sl("0:0:0:2"), sl("1:1:14:4")},
			"",
		},
		{
			"line comment at EOF",
			"id ; comment",
			[]Lexeme{Identifier("id")},
			[]SourceLocation{sl("0:0:0:2")},
			"",
		},
		{
			"empty line comment at EOF",
			"id ;",
			[]Lexeme{Identifier("id")},
			[]SourceLocation{sl("0:0:0:2")},
			"",
		},
		{
			"datum comment",
			"#;id name",
			[]Lexeme{Identifier("name")},
			[]SourceLocation{sl("0:5:5:4")},
			"",
		},
		{
			"datum comment with interlexeme space",
			"#; id name",
			[]Lexeme{Identifier("name")},
			[]SourceLocation{sl("0:6:6:4")},
			"",
		},
		{
			"datum comment at eof",
			"id #;name",
			[]Lexeme{Identifier("id")},
			[]SourceLocation{sl("0:0:0:2")},
			"",
		},
		{
			"nested comment",
			"#| comment |# id",
			[]Lexeme{Identifier("id")},
			[]SourceLocation{sl("0:14:14:2")},
			"",
		},
		{
			"nested comment at eof",
			"id #| comment |#",
			[]Lexeme{Identifier("id")},
			[]SourceLocation{sl("0:0:0:2")},
			"",
		},
		{
			"doubly nested comment",
			"#| comment #| note |# |# id",
			[]Lexeme{Identifier("id")},
			[]SourceLocation{sl("0:25:25:2")},
			"",
		},
		{
			"identifier",
			"id",
			[]Lexeme{Identifier("id")},
			[]SourceLocation{sl("0:0:0:2")},
			"",
		},
		{
			"identifier with whitespace",
			"  id ",
			[]Lexeme{Identifier("id")},
			[]SourceLocation{sl("0:2:2:2")},
			"",
		},
		{
			"two identifiers",
			"id name",
			[]Lexeme{Identifier("id"), Identifier("name")},
			[]SourceLocation{sl("0:0:0:2"), sl("0:3:3:4")},
			"",
		},
		{
			"parentheses",
			"(id)",
			[]Lexeme{LeftParenthesis{}, Identifier("id"), RightParenthesis{}},
			[]SourceLocation{sl("0:0:0:1"), sl("0:1:1:2"), sl("0:3:3:1")},
			"",
		},
		{
			"parentheses with whitespace",
			"(  id )",
			[]Lexeme{LeftParenthesis{}, Identifier("id"), RightParenthesis{}},
			[]SourceLocation{sl("0:0:0:1"), sl("0:3:3:2"), sl("0:6:6:1")},
			"",
		},
		{
			"parentheses with multiple members",
			"(id name)",
			[]Lexeme{LeftParenthesis{}, Identifier("id"), Identifier("name"), RightParenthesis{}},
			[]SourceLocation{sl("0:0:0:1"), sl("0:1:1:2"), sl("0:4:4:4"), sl("0:8:8:1")},
			"",
		},
		{
			"empty parentheses",
			"()",
			[]Lexeme{LeftParenthesis{}, RightParenthesis{}},
			[]SourceLocation{sl("0:0:0:1"), sl("0:1:1:1")},
			"",
		},
		{
			"false",
			"#f",
			[]Lexeme{Boolean(false)},
			[]SourceLocation{sl("0:0:0:2")},
			"",
		},
		{
			"false capital",
			"#F",
			[]Lexeme{Boolean(false)},
			[]SourceLocation{sl("0:0:0:2")},
			"",
		},
		{
			"true",
			"#t",
			[]Lexeme{Boolean(true)},
			[]SourceLocation{sl("0:0:0:2")},
			"",
		},
		{
			"true capital",
			"#T",
			[]Lexeme{Boolean(true)},
			[]SourceLocation{sl("0:0:0:2")},
			"",
		},
		{
			"boolean without delimiter",
			"#fn",
			nil,
			nil,
			"read: expected delimiter",
		},
		{
			"number",
			"123",
			[]Lexeme{Number("123")},
			[]SourceLocation{sl("0:0:0:3")},
			"",
		},
		{
			"character x",
			`#\x`,
			[]Lexeme{Character('x')},
			[]SourceLocation{sl("0:0:0:3")},
			"",
		},
		{
			"character hex",
			`#\x6e`,
			[]Lexeme{Character('n')},
			[]SourceLocation{sl("0:0:0:5")},
			"",
		},
		{
			"character hex capital",
			`#\x6E`,
			[]Lexeme{Character('n')},
			[]SourceLocation{sl("0:0:0:5")},
			"",
		},
		{
			"character hex with zeros",
			`#\x06e`,
			[]Lexeme{Character('n')},
			[]SourceLocation{sl("0:0:0:6")},
			"",
		},
		{
			"character hex with many zeros",
			`#\x000000006e`,
			[]Lexeme{Character('n')},
			[]SourceLocation{sl("0:0:0:13")},
			"",
		},
		{
			"character hex out of range",
			`#\x123456789abcdef`,
			nil,
			nil,
			`read: invalid hex scalar value: 123456789abcdef`,
		},
		{
			"character named",
			`#\nul`,
			[]Lexeme{Character('\x00')},
			[]SourceLocation{sl("0:0:0:5")},
			"",
		},
		{
			"character unrecognized name",
			`#\foo`,
			nil,
			nil,
			`read: unrecognized character: #\foo`,
		},
		{
			"string",
			`"name"`,
			[]Lexeme{String("name")},
			[]SourceLocation{sl("0:0:0:6")},
			"",
		},
		{
			`string escape "`,
			`"\""`,
			[]Lexeme{String(`"`)},
			[]SourceLocation{sl("0:0:0:4")},
			"",
		},
		{
			`string escape \`,
			`"\\"`,
			[]Lexeme{String(`\`)},
			[]SourceLocation{sl("0:0:0:4")},
			"",
		},
		{
			"string escape hex",
			`"\x6e;"`,
			[]Lexeme{String("n")},
			[]SourceLocation{sl("0:0:0:7")},
			"",
		},
		{
			"string escape hex unterminated",
			`"\x6e"`,
			nil,
			nil,
			"read: unexpected rune",
		},
		{
			"string escape hex empty",
			`"\x;"`,
			nil,
			nil,
			"read: unexpected delimiter",
		},
		{
			"string escape hex non hex digit",
			`"\xg;"`,
			nil,
			nil,
			"read: unexpected rune",
		},
		{
			"string line ending",
			"\"\x0a\"",
			[]Lexeme{String("\x0a")},
			[]SourceLocation{sl("0:0:0:3")},
			"",
		},
		{
			"string carriage return line ending",
			"\"\x0d\x0a\"",
			[]Lexeme{String("\x0a")},
			[]SourceLocation{sl("0:0:0:4")},
			"",
		},
		{
			"string escape intraline whitespace line ending intraline whitespace",
			"\"\\ \x0a \"",
			[]Lexeme{String("")},
			[]SourceLocation{sl("0:0:0:6")},
			"",
		},
		{
			"string escape intraline whitespace carriage return line ending intraline whitespace",
			"\"\\ \x0d\x0a \"",
			[]Lexeme{String("")},
			[]SourceLocation{sl("0:0:0:7")},
			"",
		},
		{
			"quote",
			"'a",
			[]Lexeme{Quote{}, Identifier("a")},
			[]SourceLocation{sl("0:0:0:1"), sl("0:1:1:1")},
			"",
		},
		{
			"unquote",
			"`(,a)",
			[]Lexeme{Quasiquote{}, LeftParenthesis{}, Unquote{}, Identifier("a"), RightParenthesis{}},
			[]SourceLocation{sl("0:0:0:1"), sl("0:1:1:1"), sl("0:2:2:1"), sl("0:3:3:1"), sl("0:4:4:1")},
			"",
		},
		{
			"unquote splicing",
			"`(,@a)",
			[]Lexeme{Quasiquote{}, LeftParenthesis{}, UnquoteSplicing{}, Identifier("a"), RightParenthesis{}},
			[]SourceLocation{sl("0:0:0:1"), sl("0:1:1:1"), sl("0:2:2:2"), sl("0:4:4:1"), sl("0:5:5:1")},
			"",
		},
		{
			"syntax",
			"#'a",
			[]Lexeme{Syntax{}, Identifier("a")},
			[]SourceLocation{sl("0:0:0:2"), sl("0:2:2:1")},
			"",
		},
		{
			"unsyntax",
			"#`(#,a)",
			[]Lexeme{Quasisyntax{}, LeftParenthesis{}, Unsyntax{}, Identifier("a"), RightParenthesis{}},
			[]SourceLocation{sl("0:0:0:2"), sl("0:2:2:1"), sl("0:3:3:2"), sl("0:5:5:1"), sl("0:6:6:1")},
			"",
		},
		{
			"unsyntax splicing",
			"#`(#,@a)",
			[]Lexeme{Quasisyntax{}, LeftParenthesis{}, UnsyntaxSplicing{}, Identifier("a"), RightParenthesis{}},
			[]SourceLocation{sl("0:0:0:2"), sl("0:2:2:1"), sl("0:3:3:3"), sl("0:6:6:1"), sl("0:7:7:1")},
			"",
		},
		{
			"pair",
			"(a . b)",
			[]Lexeme{LeftParenthesis{}, Identifier("a"), Dot{}, Identifier("b"), RightParenthesis{}},
			[]SourceLocation{sl("0:0:0:1"), sl("0:1:1:1"), sl("0:3:3:1"), sl("0:5:5:1"), sl("0:6:6:1")},
			"",
		},
		{
			"identifier +",
			"+",
			[]Lexeme{Identifier("+")},
			[]SourceLocation{sl("0:0:0:1")},
			"",
		},
		{
			"identifier -",
			"-",
			[]Lexeme{Identifier("-")},
			[]SourceLocation{sl("0:0:0:1")},
			"",
		},
		{
			"identifier ...",
			"...",
			[]Lexeme{Identifier("...")},
			[]SourceLocation{sl("0:0:0:3")},
			"",
		},
		{
			"identifier ->",
			"->",
			[]Lexeme{Identifier("->")},
			[]SourceLocation{sl("0:0:0:2")},
			"",
		},
		{
			"identifier -> with trailing",
			"->id",
			[]Lexeme{Identifier("->id")},
			[]SourceLocation{sl("0:0:0:4")},
			"",
		},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			lexemes, sourceLocations, err := LexString(test.source)
			if test.error == "" && err != nil {
				t.Fatal(err)
			} else if test.error != "" {
				if err == nil || err.Error() != test.error {
					t.Fatalf("\nexpected error: %v\n     got error: %v\n", test.error, err)
				}
			} else {
				if !reflect.DeepEqual(lexemes, test.lexemes) {
					t.Errorf("\nexpected: %#v\n     got: %#v", test.lexemes, lexemes)
				}
				if !reflect.DeepEqual(sourceLocations, test.sourceLocations) {
					t.Errorf("\nexpected: %#v\n     got: %#v", test.sourceLocations, sourceLocations)
				}
			}
		})
	}
}
