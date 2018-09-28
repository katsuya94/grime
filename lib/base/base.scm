(library (base)
  (export
    quote
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
  (import (core))

  (define (eq? l r)
    (eqv? l r))

  (define (equal? l r)
    (eqv? l r)))