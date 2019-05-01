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
	// create graph of markSets to markSets that contain them
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
	// check that each node has one leaf
	for i := range markSets {
		frontier := make([]bool, len(markSets))
		frontier[i] = true
		for {
			next := make([]bool, len(markSets))
			for j, in := range frontier {
				if !in {
					continue
				}
				for _, k := range graph[j] {
					next[k] = true
				}
			}
			for i := range frontier {
				if frontier[i] != next[i] {
					continue
				}
			}
			break
		}
		count := 0
		for _, in := range frontier {
			if in {
				count++
			}
		}
		if count > 1 {
			return true
		}
	}
	return false
}
