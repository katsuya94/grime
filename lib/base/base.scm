(library (base)
  (export
    quote
    syntax
    if
    let*
    begin
    lambda
    define
    define-syntax
    set!
    cons
    car
    cdr
    null?
    write
    call/cc
    error
    eqv?
    equal?)
  (import (for (core) run expand))

  (define (eq? l r)
    (eqv? l r))

  (define (equal? l r)
    (eqv? l r)))