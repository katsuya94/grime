// WARNING: this file was auto-generated by Grime

// +build js

package base

import "github.com/katsuya94/grime/runtime"

var Library *runtime.Library

func init() {
	Library = runtime.MustNewLibraryFromString("base", `
(library (base)
  (export
    quote
    syntax
    if
    begin
    lambda
    define
    define-syntax
    syntax-case
    set!
    _
    ...
    cons
    not
    car
    cdr
    null?
    pair?
    write
    call/cc
    error
    eqv?
    syntax->datum
    datum->syntax
    identifier?
    generate-temporaries
    list
    when
    unless
    let*
    letrec*
    with-syntax
    let
    letrec
    and
    or
    list?
    fold-left
    for-all
    syntax-rules
    cond
    eq?
    equal?)
  (import
    (for (only (derived) syntax-rules) expand)
    (for (except (derived) syntax-rules) run)
    (for (only (core) syntax syntax-case _ ...) expand)
    (for (only (core) set!) run expand)
    (for (except (core) syntax syntax-case _ ... set!) run))

  (define (eq? l r)
    (eqv? l r))

  (define (equal? l r)
    (eqv? l r)))
`)
}
