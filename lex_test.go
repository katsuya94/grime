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
	}{
		{
			"identifier",
			"id",
			[]Lexeme{Identifier("id")},
		},
		{
			"identifier with whitespace",
			"  id ",
			[]Lexeme{Identifier("id")},
		},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			lexemes, err := LexString(test.source)
			if err != nil {
				t.Fatal(err)
			}
			if !reflect.DeepEqual(lexemes, test.lexemes) {
				t.Fatalf("\nexpected: %v\n     got: %v", test.lexemes, lexemes)
			}
		})
	}
}
