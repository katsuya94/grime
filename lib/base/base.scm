(library (base)
  (export
    ...
    _
    and
    begin
    bound-identifier=?
    call/cc
    car
    cdr
    cond
    cons
    datum->syntax
    define
    define-syntax
    eq?
    equal?
    eqv?
    error
    fold-left
    for-all
    free-identifier=?
    generate-temporaries
    identifier?
    if
    lambda
    let
    let*
    letrec
    letrec*
    list
    list?
    not
    null?
    or
    pair?
    quote
    set!
    syntax
    syntax->datum
    syntax-case
    syntax-rules
    unless
    when
    with-syntax
    write)
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