(library (grime)
  (export
    quote
    syntax
    if
    ~let
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
    (for (except (base) syntax syntax-case set! _ ... syntax-rules) run expand)
    (for (only (core) syntax syntax-case set! _ ...) run expand)
    (for (only (derived) syntax-rules) run expand)))