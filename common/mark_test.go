package common

import (
	"testing"

	"github.com/stretchr/testify/require"
)

func TestMarkSetAddEmpty(t *testing.T) {
	marks := make([]mark, 1)
	actual := markSet{}.add(&marks[0])
	expected := markSet{&marks[0]}
	require.True(t, actual.equal(expected))
}

func TestMarkSetAddFirst(t *testing.T) {
	marks := make([]mark, 2)
	actual := markSet{&marks[1]}.add(&marks[0])
	expected := markSet{&marks[0], &marks[1]}
	require.True(t, actual.equal(expected))
}

func TestMarkSetAddEnd(t *testing.T) {
	marks := make([]mark, 2)
	actual := markSet{&marks[0]}.add(&marks[1])
	expected := markSet{&marks[0], &marks[1]}
	require.True(t, actual.equal(expected))
}

func TestMarkSetAddMiddle(t *testing.T) {
	marks := make([]mark, 3)
	actual := markSet{&marks[0], &marks[2]}.add(&marks[1])
	expected := markSet{&marks[0], &marks[1], &marks[2]}
	require.True(t, actual.equal(expected))
}

func TestMarkSetXorEmpty(t *testing.T) {
	marks := make([]mark, 1)
	actual := markSet{}.xor(&marks[0])
	expected := markSet{&marks[0]}
	require.True(t, actual.equal(expected))
}

func TestMarkSetXorExist(t *testing.T) {
	marks := make([]mark, 1)
	actual := markSet{&marks[0]}.xor(&marks[0])
	expected := markSet{}
	require.True(t, actual.equal(expected))
}

func TestMarkSetXorFirstExist(t *testing.T) {
	marks := make([]mark, 2)
	actual := markSet{&marks[0], &marks[1]}.xor(&marks[0])
	expected := markSet{&marks[1]}
	require.True(t, actual.equal(expected))
}
func TestMarkSetXorFirstNoExist(t *testing.T) {
	marks := make([]mark, 3)
	actual := markSet{&marks[1], &marks[2]}.xor(&marks[0])
	expected := markSet{&marks[0], &marks[1], &marks[2]}
	require.True(t, actual.equal(expected))
}

func TestMarkSetXorEndExist(t *testing.T) {
	marks := make([]mark, 2)
	actual := markSet{&marks[0], &marks[1]}.xor(&marks[1])
	expected := markSet{&marks[0]}
	require.True(t, actual.equal(expected))
}

func TestMarkSetXorEndNoExist(t *testing.T) {
	marks := make([]mark, 3)
	actual := markSet{&marks[0], &marks[1]}.xor(&marks[2])
	expected := markSet{&marks[0], &marks[1], &marks[2]}
	require.True(t, actual.equal(expected))
}

func TestMarkSetXorMiddleExist(t *testing.T) {
	marks := make([]mark, 3)
	actual := markSet{&marks[0], &marks[1], &marks[2]}.xor(&marks[1])
	expected := markSet{&marks[0], &marks[2]}
	require.True(t, actual.equal(expected))
}

func TestMarkSetXorMiddleNoExist(t *testing.T) {
	marks := make([]mark, 3)
	actual := markSet{&marks[0], &marks[2]}.xor(&marks[1])
	expected := markSet{&marks[0], &marks[1], &marks[2]}
	require.True(t, actual.equal(expected))
}
