package common_test

import (
	"testing"

	. "github.com/katsuya94/grime/common"
	"github.com/katsuya94/grime/test"
	"github.com/stretchr/testify/require"
)

func TestScopeList_GetEmpty(t *testing.T) {
	id := NewIdentifier(Symbol("id"))
	scopeList := NewScopeList()
	_, ok := scopeList.Get(id)
	require.False(t, ok)
}

func TestScopeList_GetOtherIdentifier(t *testing.T) {
	id := NewIdentifier(Symbol("id"))
	locations := test.Locations(1)
	scope := NewScope()
	scope.Set(id, &locations[0])
	scopeList := NewScopeList().Push(scope, 0, false)
	other := NewIdentifier(Symbol("other"))
	_, ok := scopeList.Get(other)
	require.False(t, ok)
}
func TestScopeList_GetOtherPhase(t *testing.T) {
	id := NewIdentifier(Symbol("id"))
	locations := test.Locations(1)
	scope := NewScope()
	scope.Set(id, &locations[0])
	scopeList := NewScopeList().Push(scope, 0, false)
	next, _ := id.Next().Identifier()
	_, ok := scopeList.Get(next)
	require.False(t, ok)
}

func TestScopeList_GetSamePhase(t *testing.T) {
	id := NewIdentifier(Symbol("id"))
	locations := test.Locations(1)
	scope := NewScope()
	scope.Set(id, &locations[0])
	scopeList := NewScopeList().Push(scope, 0, false)
	bindingStackContext, ok := scopeList.Get(id)
	require.True(t, ok)
	require.True(t, bindingStackContext.Binding.(*Variable) == &locations[0])
	require.Equal(t, CurrentStackContext, bindingStackContext.StackContext)
}

func TestScopeList_GetLexical(t *testing.T) {
	id := NewIdentifier(Symbol("id"))
	locations := test.Locations(1)
	scope := NewScope()
	scope.Set(id, &locations[0])
	scopeList := NewScopeList().Push(scope, LEXICAL, false)
	next, _ := id.Next().Identifier()
	bindingStackContext, ok := scopeList.Get(next)
	require.True(t, ok)
	require.True(t, bindingStackContext.Binding.(*Variable) == &locations[0])
	require.Equal(t, CurrentStackContext, bindingStackContext.StackContext)
}

func TestScopeList_GetMany(t *testing.T) {
	id := NewIdentifier(Symbol("id"))
	locations := test.Locations(1)
	outer := NewScope()
	outer.Set(id, &locations[0])
	inner := NewScope()
	scopeList := NewScopeList().Push(outer, 0, false).Push(inner, 0, false)
	bindingStackContext, ok := scopeList.Get(id)
	require.True(t, ok)
	require.True(t, bindingStackContext.Binding.(*Variable) == &locations[0])
	require.Equal(t, CurrentStackContext, bindingStackContext.StackContext)
}

func TestScopeList_GetShadow(t *testing.T) {
	id := NewIdentifier(Symbol("id"))
	locations := test.Locations(2)
	outer := NewScope()
	outer.Set(id, &locations[0])
	inner := NewScope()
	inner.Set(id, &locations[1])
	scopeList := NewScopeList().Push(outer, 0, false).Push(inner, 0, false)
	bindingStackContext, ok := scopeList.Get(id)
	require.True(t, ok)
	require.True(t, bindingStackContext.Binding.(*Variable) == &locations[1])
	require.Equal(t, CurrentStackContext, bindingStackContext.StackContext)
}

func TestScopeList_GetFrame(t *testing.T) {
	id := NewIdentifier(Symbol("id"))
	locations := test.Locations(1)
	scope := NewScope()
	scope.Set(id, &locations[0])
	scopeList := NewScopeList().Push(scope, LEXICAL, true)
	bindingStackContext, ok := scopeList.Get(id)
	require.True(t, ok)
	require.True(t, bindingStackContext.Binding.(*Variable) == &locations[0])
	require.Equal(t, CurrentStackContext, bindingStackContext.StackContext)
}

func TestScopeList_GetFrameNext(t *testing.T) {
	id := NewIdentifier(Symbol("id"))
	locations := test.Locations(1)
	outer := NewScope()
	outer.Set(id, &locations[0])
	inner := NewScope()
	scopeList := NewScopeList().Push(outer, 0, false).Push(inner, 0, true)
	bindingStackContext, ok := scopeList.Get(id)
	require.True(t, ok)
	require.True(t, bindingStackContext.Binding.(*Variable) == &locations[0])
	require.Equal(t, StackContext(1), bindingStackContext.StackContext)
}
