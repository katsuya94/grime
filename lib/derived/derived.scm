(library (derived)
  (export
    when unless let* let and or list? fold-left for-all syntax-rules)
  (import
    (core)
    (for (only (core) ~let) expand))

  (define-syntax when
    (lambda (x)
      (syntax-case x ()
        [(_ test body ...) #'(if test (begin body ...) #f)])))

  (define-syntax unless
    (lambda (x)
      (syntax-case x ()
        [(_ test body ...) #'(when (not test) body ...)])))

  (define-syntax let*
    (lambda (x)
      (syntax-case x ()
        [(_ () body ...) #'(begin body ...)]
        [(_ ((v0 e0) (v e) ...) body ...)
         #'(~let (v0 e0) (let* ((v e) ...) body ...))])))

  (define-syntax let
    (lambda (x)
      (syntax-case x ()
        [(_ () body ...) #'(let* () body ...)]
        [(_ ((v e)) body ...) #'(let* ((v e)) body ...)]
        [(_ ((v0 e0) (v e) ...) body ...)
         #'(let*
             ((x e0))
             (let ((v e) ...) (let* ((v0 x)) body ...)))])))

  (define-syntax and
    (lambda (x)
      (syntax-case x ()
        [(_ ) #'#t]
        [(_ e) #'e]
        [(_ e1 e2 e3 ...)
         #'(let ([t e1])
             (if t (and e2 e3 ...) t))])))

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