package r6rs_test

import (
	"testing"

	. "github.com/katsuya94/grime/lib/r6rs"
	"github.com/katsuya94/grime/test"
	"github.com/stretchr/testify/assert"
)

func TestEqualSameIdentifierTrue(t *testing.T) {
	a := Introduce(test.Syntax("id"))
	b := Introduce(test.Syntax("id"))
	assert.True(t, Equal(a, b))
}

func TestEqualDifferentIdentifierTrue(t *testing.T) {
	a := Introduce(test.Syntax("id"))
	b := Introduce(test.Syntax("name"))
	assert.True(t, Equal(a, b))
}

func TestEqualSameCoreTrue(t *testing.T) {
	a := Introduce(test.Syntax("#%literal"))
	b := Introduce(test.Syntax("#%literal"))
	assert.True(t, Equal(a, b))
}

func TestEqualDifferentCoreFalse(t *testing.T) {
	a := Introduce(test.Syntax("#%literal"))
	b := Introduce(test.Syntax("#%load"))
	assert.False(t, Equal(a, b))
}

func TestEqualPairSameTrue(t *testing.T) {
	a := Introduce(test.Syntax("(idA . nameA)"))
	b := Introduce(test.Syntax("(idB . nameB)"))
	assert.True(t, Equal(a, b))
}

func TestEqualPairDifferentFirstFalse(t *testing.T) {
	a := Introduce(test.Syntax("(id . name)"))
	b := Introduce(test.Syntax("(#f . name)"))
	assert.False(t, Equal(a, b))
}

func TestEqualPairDifferentRestFalse(t *testing.T) {
	a := Introduce(test.Syntax("(id . name)"))
	b := Introduce(test.Syntax("(id . #f)"))
	assert.False(t, Equal(a, b))
}

func TestEqualPairSameMatchTrue(t *testing.T) {
	a := Introduce(test.Syntax("(id . id)"))
	b := Introduce(test.Syntax("(name . name)"))
	assert.True(t, Equal(a, b))
}

func TestEqualPairDifferentMatchAFalse(t *testing.T) {
	a := Introduce(test.Syntax("(id . other)"))
	b := Introduce(test.Syntax("(name . name)"))
	assert.False(t, Equal(a, b))
}

func TestEqualPairDifferentMatchBFalse(t *testing.T) {
	a := Introduce(test.Syntax("(id . id)"))
	b := Introduce(test.Syntax("(name . other)"))
	assert.False(t, Equal(a, b))
}

func TestEqualWrappedSameTrue(t *testing.T) {
	a := Introduce(test.Syntax("#f"))
	b := Introduce(test.Syntax("#f"))
	assert.True(t, Equal(a, b))
}

func TestEqualWrappedSameFalse(t *testing.T) {
	a := Introduce(test.Syntax("#t"))
	b := Introduce(test.Syntax("#f"))
	assert.False(t, Equal(a, b))
}
