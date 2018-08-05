package grime

import (
	"testing"
	"reflect"
)

func TestLex(t *testing.T) {
	tests := []struct {
		name string
		source string
		lexemes []Lexeme
		error string
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
			`expected delimiter; got "n"`,
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
				t.Errorf("\nexpected: %v\n     got: %v", test.lexemes, lexemes)
			}
		})
	}
}
