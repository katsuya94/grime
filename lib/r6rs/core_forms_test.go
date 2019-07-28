package r6rs_test

import (
	"testing"

	"github.com/katsuya94/grime/common"
	. "github.com/katsuya94/grime/lib/r6rs"
	"github.com/katsuya94/grime/test"
	"github.com/stretchr/testify/assert"
)

func TestLiteralForm_Unexpand(t *testing.T) {
	coreForm := LiteralForm{Datum: common.Boolean(true)}
	actual := coreForm.Unexpand()
	expected := Introduce(test.Syntax("(#%literal #t)"))
	assert.Equal(t, expected, actual)
}
