package core

import "testing"

func TestString(t *testing.T) {
	tests := []struct {
		name     string
		datum    Datum
		expected string
	}{
		{
			"false",
			Boolean(false),
			"#f",
		},
		{
			"true",
			Boolean(true),
			"#t",
		},
		{
			"number",
			Number("123"),
			"123",
		},
		{
			"character",
			Character('a'),
			`#\a`,
		},
		{
			"string",
			String("name"),
			`"name"`,
		},
		{
			"symbol",
			Symbol("id"),
			"'id",
		},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			if actual := test.datum.String(); actual != test.expected {
				t.Errorf("\nexpected: %v\n     got: %v", test.expected, actual)
			}
		})
	}
}
