package eval

import (
	"testing"
	"github.com/katsuya94/grime/core"
	"reflect"
	"github.com/katsuya94/grime/read"
)

func TestEvaluate(t *testing.T) {
	tests := []struct {
		name   string
		source string
		data   []core.Datum
		error  string
	}{
		{
			"boolean",
			"#f",
			[]core.Datum{core.Boolean(false)},
			"",
		},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			body, err := read.ReadString(test.source)

			if test.error == "" && err != nil {
				t.Fatal(err)
			} else if test.error != "" && err != nil && err.Error() != test.error {
				t.Errorf("\nexpected error: %v\n     got error: %v\n", test.error, err.Error())
			}
			if test.data != nil && !reflect.DeepEqual(data, test.data) {
				t.Errorf("\nexpected: %#v\n     got: %#v", test.data, data)
			}
		})
	}
}

