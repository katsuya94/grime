package read

import (
	"testing"
	"reflect"
)

func TestRead(t *testing.T) {
	tests := []struct {
		name    string
		source  string
		data []Datum
		error   string
	}{
		{
			"identifier",
			"id",
			[]Datum{Identifier("id")},
			"",
		},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			data, err := ReadString(test.source)
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

