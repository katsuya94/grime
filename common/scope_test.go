package common_test

import (
	"testing"

	. "github.com/katsuya94/grime/common"
	"github.com/stretchr/testify/require"
)

type comparableLocation struct {
	*comparableLocation
}

func location() Location {
	c := &comparableLocation{}
	c.comparableLocation = c
	return c
}

func TestScopeList_GetEmpty(t *testing.T) {
	id := NewIdentifier(Symbol("id"))
	scopeList := NewScopeList()
	require.Nil(t, scopeList.Get(id))
}

func TestScopeList_GetOtherIdentifier(t *testing.T) {
	id := NewIdentifier(Symbol("id"))
	location := location()
	scope := NewScope()
	scope.Set(id, location)
	scopeList := NewScopeList().Push(scope, 0)
	other := NewIdentifier(Symbol("other"))
	require.Nil(t, scopeList.Get(other))
}
func TestScopeList_GetOtherPhase(t *testing.T) {
	id := NewIdentifier(Symbol("id"))
	location := location()
	scope := NewScope()
	scope.Set(id, location)
	scopeList := NewScopeList().Push(scope, 0)
	other, _ := id.Next().Identifier()
	require.Nil(t, scopeList.Get(other))
}

func TestScopeList_GetSamePhase(t *testing.T) {
	id := NewIdentifier(Symbol("id"))
	location := location()
	scope := NewScope()
	scope.Set(id, location)
	scopeList := NewScopeList().Push(scope, 0)
	require.Exactly(t, location, scopeList.Get(id))
}

func TestScopeList_GetLexical(t *testing.T) {
	id := NewIdentifier(Symbol("id"))
	location := location()
	scope := NewScope()
	scope.Set(id, location)
	scopeList := NewScopeList().Push(scope, LEXICAL)
	next, _ := id.Next().Identifier()
	require.Exactly(t, location, scopeList.Get(next))
}

func TestScopeList_GetMany(t *testing.T) {
	id := NewIdentifier(Symbol("id"))
	location := location()
	outer := NewScope()
	outer.Set(id, location)
	inner := NewScope()
	scopeList := NewScopeList().Push(outer, 0).Push(inner, 0)
	require.Exactly(t, location, scopeList.Get(id))
}

func TestScopeList_GetShadow(t *testing.T) {
	id := NewIdentifier(Symbol("id"))
	other := location()
	location := location()
	outer := NewScope()
	outer.Set(id, other)
	inner := NewScope()
	inner.Set(id, location)
	scopeList := NewScopeList().Push(outer, 0).Push(inner, 0)
	require.Exactly(t, location, scopeList.Get(id))
}
