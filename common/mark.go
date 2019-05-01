package common

import (
	"unsafe"
)

type Marker interface {
	Datum
	Mark(m *M) Marker
}

type M struct {
	*M
}

func NewMark() *M {
	m := &M{}
	m.M = m
	return m
}

type markSet []*M

func (s markSet) xor(m *M) markSet {
	i := 0
	j := len(s)
	for {
		if i == j {
			if i < len(s) && m == s[i] {
				new := make(markSet, len(s)-1)
				copy(new[:i], s[:i])
				copy(new[i:], s[i+1:])
				return new
			}
			new := make(markSet, len(s)+1)
			copy(new[:i], s[:i])
			new[i] = m
			copy(new[i+1:], s[i:])
			return new
		}
		k := i + (j-i)/2
		if uintptr(unsafe.Pointer(m)) <= uintptr(unsafe.Pointer(s[k])) {
			j = k
		} else if uintptr(unsafe.Pointer(m)) > uintptr(unsafe.Pointer(s[k])) {
			i = k + 1
		}
	}
}

func (s markSet) equal(other markSet) bool {
	if len(s) != len(other) {
		return false
	}
	for i := range s {
		if s[i] != other[i] {
			return false
		}
	}
	return true
}

func (s markSet) contains(other markSet) bool {
	if len(other) > len(s) {
		return false
	}
	i := 0
	for _, m := range other {
		found := false
		for ; i < len(s); i++ {
			if m == s[i] {
				found = true
				break
			}
		}
		if !found {
			return false
		}
	}
	return true
}

// TODO: improve this algorithm
func duplicateMarkSets(markSets ...markSet) bool {
	// create graph of markSets to markSets that contain them, excluding self
	graph := make([][]int, len(markSets))
	for i, markSet := range markSets {
		graph[i] = make([]int, 0, len(markSets))
		for j, maybeContainsMarkSet := range markSets {
			if i == j {
				continue
			}
			if maybeContainsMarkSet.contains(markSet) {
				graph[i] = append(graph[i], j)
			}
		}
	}
	// if there is a a cycle or more than one leaf, there are duplicates
	for i := range markSets {
		visited := make([]int, len(markSets))
		dfs()
	}
	return false
}
