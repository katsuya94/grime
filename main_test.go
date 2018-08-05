package grime

/*func TestParse(t *testing.T) {
	tests := []struct {
		name string
		source string
		ast AST
	}{
	  {
	  	"hello world",
	  	`(print "hello world")`,
		  &Pairr{intern("print"), &Pairr{String("hello world"), nil}},
	  },
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			ast, error := Parse(test.source)
			if error != nil {
				t.Fatal(error)
			}
			if !ast.Equal(test.ast) {
				t.Fatalf("expected: %v\n     got: %v", test.ast, ast)
			}
		})
	}
}*/
