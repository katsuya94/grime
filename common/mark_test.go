package common

import (
	"testing"

	"github.com/stretchr/testify/require"
)

func TestMarkSetXorEmpty(t *testing.T) {
	marks := make([]M, 1)
	actual := markSet{}.xor(&marks[0])
	expected := markSet{&marks[0]}
	require.True(t, actual.equal(expected))
}

func TestMarkSetXorExist(t *testing.T) {
	marks := make([]M, 1)
	actual := markSet{&marks[0]}.xor(&marks[0])
	expected := markSet{}
	require.True(t, actual.equal(expected))
}

func TestMarkSetXorFirstExist(t *testing.T) {
	marks := make([]M, 2)
	actual := markSet{&marks[0], &marks[1]}.xor(&marks[0])
	expected := markSet{&marks[1]}
	require.True(t, actual.equal(expected))
}
func TestMarkSetXorFirstNoExist(t *testing.T) {
	marks := make([]M, 3)
	actual := markSet{&marks[1], &marks[2]}.xor(&marks[0])
	expected := markSet{&marks[0], &marks[1], &marks[2]}
	require.True(t, actual.equal(expected))
}

func TestMarkSetXorEndExist(t *testing.T) {
	marks := make([]M, 2)
	actual := markSet{&marks[0], &marks[1]}.xor(&marks[1])
	expected := markSet{&marks[0]}
	require.True(t, actual.equal(expected))
}

func TestMarkSetXorEndNoExist(t *testing.T) {
	marks := make([]M, 3)
	actual := markSet{&marks[0], &marks[1]}.xor(&marks[2])
	expected := markSet{&marks[0], &marks[1], &marks[2]}
	require.True(t, actual.equal(expected))
}

func TestMarkSetXorMiddleExist(t *testing.T) {
	marks := make([]M, 3)
	actual := markSet{&marks[0], &marks[1], &marks[2]}.xor(&marks[1])
	expected := markSet{&marks[0], &marks[2]}
	require.True(t, actual.equal(expected))
}

func TestMarkSetXorMiddleNoExist(t *testing.T) {
	marks := make([]M, 3)
	actual := markSet{&marks[0], &marks[2]}.xor(&marks[1])
	expected := markSet{&marks[0], &marks[1], &marks[2]}
	require.True(t, actual.equal(expected))
}

func TestMarkSetSubsetEmptyEmpty(t *testing.T) {
	require.True(t, markSet{}.subset(markSet{}))
}

func TestMarkSetSubsetNonEmptyEmpty(t *testing.T) {
	marks := make([]M, 1)
	require.True(t, markSet{&marks[0]}.subset(markSet{}))
}

func TestMarkSetSubsetIdentical(t *testing.T) {
	marks := make([]M, 1)
	require.True(t, markSet{&marks[0]}.subset(markSet{&marks[0]}))
}

func TestMarkSetSubsetFirstMissing(t *testing.T) {
	marks := make([]M, 2)
	require.True(t, markSet{&marks[0], &marks[1]}.subset(markSet{&marks[1]}))
}

func TestMarkSetSubsetLastMissing(t *testing.T) {
	marks := make([]M, 2)
	require.True(t, markSet{&marks[0], &marks[1]}.subset(markSet{&marks[0]}))
}

func TestMarkSetSubsetMiddleMissing(t *testing.T) {
	marks := make([]M, 3)
	require.True(t, markSet{&marks[0], &marks[1], &marks[2]}.subset(markSet{&marks[0], &marks[2]}))
}

func TestMarkSetSubsetEmptyNonEmpty(t *testing.T) {
	marks := make([]M, 1)
	require.False(t, markSet{}.subset(markSet{&marks[0]}))
}

func TestMarkSetSubsetFirstExtra(t *testing.T) {
	marks := make([]M, 2)
	require.False(t, markSet{&marks[1]}.subset(markSet{&marks[0], &marks[1]}))
}

func TestMarkSetSubsetLastExtra(t *testing.T) {
	marks := make([]M, 2)
	require.False(t, markSet{&marks[0]}.subset(markSet{&marks[0], &marks[1]}))
}

func TestMarkSetSubsetMiddleExtra(t *testing.T) {
	marks := make([]M, 3)
	require.False(t, markSet{&marks[0], &marks[2]}.subset(markSet{&marks[0], &marks[1], &marks[2]}))
}
