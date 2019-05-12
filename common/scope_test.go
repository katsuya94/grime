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
	bindingStackContext := scopeList.Get(id)
	require.True(t, bindingStackContext.Nil())
}

func TestScopeList_GetOtherIdentifier(t *testing.T) {
	id := NewIdentifier(Symbol("id"))
	bindings := test.Bindings(1)
	scope := NewScope()
	scope.Set(id, &bindings[0])
	scopeList := NewScopeList().Push(scope, 0, false)
	other := NewIdentifier(Symbol("other"))
	bindingStackContext := scopeList.Get(other)
	require.True(t, bindingStackContext.Nil())
}
func TestScopeList_GetOtherPhase(t *testing.T) {
	id := NewIdentifier(Symbol("id"))
	bindings := test.Bindings(1)
	scope := NewScope()
	scope.Set(id, &bindings[0])
	scopeList := NewScopeList().Push(scope, 0, false)
	next, _ := id.Next().Identifier()
	bindingStackContext := scopeList.Get(next)
	require.True(t, bindingStackContext.Nil())
}

func TestScopeList_GetSamePhase(t *testing.T) {
	id := NewIdentifier(Symbol("id"))
	bindings := test.Bindings(1)
	scope := NewScope()
	scope.Set(id, &bindings[0])
	scopeList := NewScopeList().Push(scope, 0, false)
	bindingStackContext := scopeList.Get(id)
	require.True(t, bindingStackContext.Binding.(*Variable) == &bindings[0])
	require.Equal(t, CurrentStackContext, bindingStackContext.StackContext)
}

func TestScopeList_GetLexical(t *testing.T) {
	id := NewIdentifier(Symbol("id"))
	bindings := test.Bindings(1)
	scope := NewScope()
	scope.Set(id, &bindings[0])
	scopeList := NewScopeList().Push(scope, LEXICAL, false)
	next, _ := id.Next().Identifier()
	bindingStackContext := scopeList.Get(next)
	require.True(t, bindingStackContext.Binding.(*Variable) == &bindings[0])
	require.Equal(t, CurrentStackContext, bindingStackContext.StackContext)
}

func TestScopeList_GetMany(t *testing.T) {
	id := NewIdentifier(Symbol("id"))
	bindings := test.Bindings(1)
	outer := NewScope()
	outer.Set(id, &bindings[0])
	inner := NewScope()
	scopeList := NewScopeList().Push(outer, 0, false).Push(inner, 0, false)
	bindingStackContext := scopeList.Get(id)
	require.True(t, bindingStackContext.Binding.(*Variable) == &bindings[0])
	require.Equal(t, CurrentStackContext, bindingStackContext.StackContext)
}

func TestScopeList_GetShadow(t *testing.T) {
	id := NewIdentifier(Symbol("id"))
	bindings := test.Bindings(2)
	outer := NewScope()
	outer.Set(id, &bindings[0])
	inner := NewScope()
	inner.Set(id, &bindings[1])
	scopeList := NewScopeList().Push(outer, 0, false).Push(inner, 0, false)
	bindingStackContext := scopeList.Get(id)
	require.True(t, bindingStackContext.Binding.(*Variable) == &bindings[1])
	require.Equal(t, CurrentStackContext, bindingStackContext.StackContext)
}

func TestScopeList_GetFrame(t *testing.T) {
	id := NewIdentifier(Symbol("id"))
	bindings := test.Bindings(1)
	scope := NewScope()
	scope.Set(id, &bindings[0])
	scopeList := NewScopeList().Push(scope, LEXICAL, true)
	bindingStackContext := scopeList.Get(id)
	require.True(t, bindingStackContext.Binding.(*Variable) == &bindings[0])
	require.Equal(t, CurrentStackContext, bindingStackContext.StackContext)
}

func TestScopeList_GetFrameNext(t *testing.T) {
	id := NewIdentifier(Symbol("id"))
	bindings := test.Bindings(1)
	outer := NewScope()
	outer.Set(id, &bindings[0])
	inner := NewScope()
	scopeList := NewScopeList().Push(outer, 0, false).Push(inner, 0, true)
	bindingStackContext := scopeList.Get(id)
	require.True(t, bindingStackContext.Binding.(*Variable) == &bindings[0])
	require.Equal(t, StackContext(1), bindingStackContext.StackContext)
}
