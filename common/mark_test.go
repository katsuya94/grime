package common

import (
	"testing"

	"github.com/stretchr/testify/require"
)

func TestMarkSet_XorEmpty(t *testing.T) {
	marks := make([]M, 1)
	actual := markSet{}.xor(&marks[0])
	expected := markSet{&marks[0]}
	require.True(t, actual.equal(expected))
}

func TestMarkSet_XorExist(t *testing.T) {
	marks := make([]M, 1)
	actual := markSet{&marks[0]}.xor(&marks[0])
	expected := markSet{}
	require.True(t, actual.equal(expected))
}

func TestMarkSet_XorFirstExist(t *testing.T) {
	marks := make([]M, 2)
	actual := markSet{&marks[0], &marks[1]}.xor(&marks[0])
	expected := markSet{&marks[1]}
	require.True(t, actual.equal(expected))
}
func TestMarkSet_XorFirstNoExist(t *testing.T) {
	marks := make([]M, 3)
	actual := markSet{&marks[1], &marks[2]}.xor(&marks[0])
	expected := markSet{&marks[0], &marks[1], &marks[2]}
	require.True(t, actual.equal(expected))
}

func TestMarkSet_XorEndExist(t *testing.T) {
	marks := make([]M, 2)
	actual := markSet{&marks[0], &marks[1]}.xor(&marks[1])
	expected := markSet{&marks[0]}
	require.True(t, actual.equal(expected))
}

func TestMarkSet_XorEndNoExist(t *testing.T) {
	marks := make([]M, 3)
	actual := markSet{&marks[0], &marks[1]}.xor(&marks[2])
	expected := markSet{&marks[0], &marks[1], &marks[2]}
	require.True(t, actual.equal(expected))
}

func TestMarkSet_XorMiddleExist(t *testing.T) {
	marks := make([]M, 3)
	actual := markSet{&marks[0], &marks[1], &marks[2]}.xor(&marks[1])
	expected := markSet{&marks[0], &marks[2]}
	require.True(t, actual.equal(expected))
}

func TestMarkSet_XorMiddleNoExist(t *testing.T) {
	marks := make([]M, 3)
	actual := markSet{&marks[0], &marks[2]}.xor(&marks[1])
	expected := markSet{&marks[0], &marks[1], &marks[2]}
	require.True(t, actual.equal(expected))
}

func TestMarkSet_EqualEmpty(t *testing.T) {
	left := markSet{}
	right := markSet{}
	require.True(t, left.equal(right))
}

func TestMarkSet_EqualNonEmptyEmpty(t *testing.T) {
	marks := make([]M, 1)
	left := markSet{&marks[0]}
	right := markSet{}
	require.False(t, left.equal(right))
}
func TestMarkSet_EqualNonEmptySame(t *testing.T) {
	marks := make([]M, 1)
	left := markSet{&marks[0]}
	right := markSet{&marks[0]}
	require.True(t, left.equal(right))
}
func TestMarkSet_EqualFirstDifferent(t *testing.T) {
	marks := make([]M, 3)
	left := markSet{&marks[0], &marks[1]}
	right := markSet{&marks[1], &marks[2]}
	require.False(t, left.equal(right))
}

func TestMarkSet_EqualLastDifferent(t *testing.T) {
	marks := make([]M, 3)
	left := markSet{&marks[1], &marks[2]}
	right := markSet{&marks[0], &marks[1]}
	require.False(t, left.equal(right))
}

func TestMarkSet_SubsetEmptyEmpty(t *testing.T) {
	require.True(t, markSet{}.contains(markSet{}))
}

func TestMarkSet_SubsetNonEmptyEmpty(t *testing.T) {
	marks := make([]M, 1)
	require.True(t, markSet{&marks[0]}.contains(markSet{}))
}

func TestMarkSet_SubsetIdentical(t *testing.T) {
	marks := make([]M, 1)
	require.True(t, markSet{&marks[0]}.contains(markSet{&marks[0]}))
}

func TestMarkSet_SubsetFirstMissing(t *testing.T) {
	marks := make([]M, 2)
	require.True(t, markSet{&marks[0], &marks[1]}.contains(markSet{&marks[1]}))
}

func TestMarkSet_SubsetLastMissing(t *testing.T) {
	marks := make([]M, 2)
	require.True(t, markSet{&marks[0], &marks[1]}.contains(markSet{&marks[0]}))
}

func TestMarkSet_SubsetMiddleMissing(t *testing.T) {
	marks := make([]M, 3)
	require.True(t, markSet{&marks[0], &marks[1], &marks[2]}.contains(markSet{&marks[0], &marks[2]}))
}

func TestMarkSet_SubsetEmptyNonEmpty(t *testing.T) {
	marks := make([]M, 1)
	require.False(t, markSet{}.contains(markSet{&marks[0]}))
}

func TestMarkSet_SubsetFirstExtra(t *testing.T) {
	marks := make([]M, 2)
	require.False(t, markSet{&marks[1]}.contains(markSet{&marks[0], &marks[1]}))
}

func TestMarkSet_SubsetLastExtra(t *testing.T) {
	marks := make([]M, 2)
	require.False(t, markSet{&marks[0]}.contains(markSet{&marks[0], &marks[1]}))
}

func TestMarkSet_SubsetMiddleExtra(t *testing.T) {
	marks := make([]M, 3)
	require.False(t, markSet{&marks[0], &marks[2]}.contains(markSet{&marks[0], &marks[1], &marks[2]}))
}
