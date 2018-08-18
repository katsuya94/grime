package lex

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
type Quasiquote struct{}
type Unquote struct{}
type UnquoteSplicing struct{}
type Syntax struct{}
type Quasisyntax struct{}
type Unsyntax struct{}
type UnsyntaxSplicing struct{}
type Dot struct{}
