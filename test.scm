(import
  (for (only (core)
         define-syntax
         write)
       run)
  (for (only (core)
         ...
         _
         begin
         generate-temporaries
         identifier?
         if
         lambda
         list
         set!
         syntax
         syntax-case
         ~let)
       expand))

(define-syntax let*
  (lambda (x)
    (syntax-case x ()
      [(_ () b1 b2 ...) #'(begin b1 b2 ...)]
      [(_ ((i1 e1) (i2 e2) ...) b1 b2 ...)
        #'(~let (i1 e1) (let* ((i2 e2) ...) b1 b2 ...))])))

(define-syntax letrec*
  (lambda (x)
    (syntax-case x ()
      [(_ () b1 b2 ...) #'(begin b1 b2 ...)]
      [(_ ((i1 e1) (i2 e2) ...) b1 b2 ...)
        #'(~let (i1 #f) (set! i1 e1) (let* ((i2 e2) ...) b1 b2 ...))])))

(define-syntax with-syntax
  (lambda (x)
    (syntax-case x ()
      [(_ ((p e0) ...) e1 e2 ...)
        #'(syntax-case (list e0 ...) ()
            ((p ...) (begin e1 e2 ...)))])))

(define-syntax let
  (lambda (x)
    (syntax-case x ()
      [(_ v ((i e) ...) b1 b2 ...)
        (identifier? #'v)
        #'(letrec* ((v (lambda (i ...) b1 b2 ...)))
            (let ((i e) ...) b1 b2 ...))]
      [(_ ((i e) ...) b1 b2 ...)
        (with-syntax
          ([(t ...) (generate-temporaries #'(i ...))])
          #'(let* ((t e) ...)
              (let* ((i t) ...) b1 b2 ...)))])))

(define-syntax and
  (lambda (x)
    (syntax-case x ()
      [(_ ) #'#t]
      [(_ e) #'e]
      [(_ e1 e2 e3 ...)
       #'(let ([t e1])
           (if t (and e2 e3 ...) t))])))

(write (and #t 'foo))