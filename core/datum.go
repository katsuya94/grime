package core

import "fmt"

type Datum interface{
	String() string
	Quote()  string
}

type Boolean bool

func (b Boolean) String() string {
	if b {
		return "#t"
	} else {
		return "#f"
	}
}

func (b Boolean) Quote() string {
	return b.String()
}

type Number string

func (n Number) String() string {
	return string(n)
}

func (n Number) Quote() string {
	return n.String()
}

type Character rune

func (c Character) String() string {
	return fmt.Sprintf(`#\%v`, string(c))
}

func (c Character) Quote() string {
	return c.String()
}

type String string

func (s String) String() string {
	return fmt.Sprintf(`"%v"`, string(s))
}

func (s String) Quote() string {
	return s.String()
}

type Symbol string

func (s Symbol) String() string {
	return s
}

func (s String) Quote() string {
	return fmt.Sprintf("'%v", s.String())
}

type Pair struct {
	First Datum
	Rest  Datum
}

func (p Pair) String() string {
	s := "("
	for {
		f := p.First.String()
		if r, ok := p.Rest.(Pair); ok {
			s +=
		} else if p.Rest == nil {
			s += fmt.Sprintf")"
			break
		}
	}
}

func (p Pair) Quote() string {
	return fmt.Sprintf("'%v", p.String())
}
