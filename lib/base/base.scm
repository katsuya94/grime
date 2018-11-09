(library (base)
  (export
    quote
    syntax
    if
    ~let ; TODO remove after implementing syntax hygiene
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
    identifier?
    when
    unless
    let*
    with-syntax
    let
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
    (for (except (core) syntax syntax-case _ ...) run))

  (define (eq? l r)
    (eqv? l r))

  (define (equal? l r)
    (eqv? l r)))