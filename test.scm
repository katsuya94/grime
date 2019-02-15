(import
  (for (only (core)
         define-syntax
         quote
         syntax
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

(define-syntax with-syntax
  (lambda (x)
    (syntax-case x ()
      [(_ ((p r0) ...) r1 r2 ...)
        #'(syntax-case (list r0 ...) ()
            ((p ...) (begin r1 r2 ...)))])))
(define-syntax asdxfsdfadfs
  (lambda (x)
    (syntax-case x ()
      [(_ ((i e) ...) b1 b2 ...)
        (with-syntax
          ([(t ...) (generate-temporaries #'(i ...))])
          #'(let* ((i e) ...)
              b1 b2 ...))])))

(write #'(t ... ... ... ...))
#|
(define-syntax and
  (lambda (x)
    (syntax-case x ()
      [(_ ) #'#t]
      [(_ e) #'e]
      [(_ e1 e2 e3 ...)
       #'(let* ([t ... e2])
           (if t ... (and e2 e3 ...) t ...))])))

(write (and #t 'foo))
|#