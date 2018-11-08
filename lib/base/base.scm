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
    syntax-case
    set!
    _
    ...
    cons
    car
    cdr
    null?
    write
    call/cc
    error
    eqv?
    syntax->datum
    identifier?
    eq?
    equal?
    or
    list?
    fold-left
    for-all
    syntax-rules)
  (import (for (core) run expand))

  (define (eq? l r)
    (eqv? l r))

  (define (equal? l r)
    (eqv? l r))

  (define-syntax when
    (lambda (x)
      (syntax-case x ()
        [(_ test body ...) #'(if test (begin body ...) #f)])))

  (define-syntax unless
    (lambda (x)
      (syntax-case x ()
        [(_ test body ...) #'(when (not test) body ...)])))

  (define-syntax or
    (lambda (x)
      (syntax-case x ()
        [(_ ) #'#f]
        [(_ e) #'e]
        [(_ e1 e2 e3 ...)
         #'(let ([t e1])
         (if t t (or e2 e3 ...)))])))
  
  (define (list? x)
    (or (null? x) (pair? x)))
  
  (define (fold-left combine nil lst)
    (unless (proc? combine) (error "fold-left: expected proc"))
    (unless (list? lst) (error "fold-left: expected list"))
    (if (null? lst)
      nil
      (fold-left combine (combine nil (car lst)) (cdr lst))))

  (define (for-all proc lst)
    (fold-left (lambda (b x) (and b (proc x))) #t lst))

  (define-syntax syntax-rules
    (lambda (x)
      (syntax-case x ()
        [(_ (lit ...) [(k . p) t] ...)
         (for-all identifier? #'(lit ... k ...))
         #'(lambda (x)
             (syntax-case x (lit ...)
               [(_ . p) #'t] ...))]))))