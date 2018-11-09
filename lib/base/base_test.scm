(import (base))

(define (assert-equal actual expected)
  (unless (equal? actual expected) #t (error "assertion failed")))

(define x #f)
(when #t (set! x #t))
(assert-equal x #t)
