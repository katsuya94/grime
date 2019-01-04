package common

import (
	"unsafe"
)

type mark struct {
	SourceLocation
}

type markSet []*mark

func (s markSet) add(m *mark) markSet {
	i := 0
	j := len(s)
	for {
		if i == j {
			new := make(markSet, len(s)+1)
			copy(new[:i], s[:i])
			new[i] = m
			copy(new[i+1:], s[i:])
			return new
		}
		k := (j - i) / 2
		if uintptr(unsafe.Pointer(m)) < uintptr(unsafe.Pointer(s[k])) {
			j = k
		} else if uintptr(unsafe.Pointer(m)) > uintptr(unsafe.Pointer(s[k])) {
			i = k + 1
		} else {
			panic("duplicate mark")
		}
	}
}

func (s markSet) xor(m *mark) markSet {
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
		k := (j - i) / 2
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
