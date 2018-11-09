(import (grime))

(define (assert-equal actual expected)
  (unless (equal? actual expected) #t (error "assertion failed")))

(define x #f)

(set! x #f)
(when #t (set! x #t))
(assert-equal x #t)

(set! x #f)
(when #f (set! x #t))
(assert-equal x #f)

(set! x #f)
(unless #t (set! x #t))
(assert-equal x #f)

(set! x #f)
(unless #f (set! x #t))
(assert-equal x #t)