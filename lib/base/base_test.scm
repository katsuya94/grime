(import (base))

(define (assert-equal actual expected)
  (if (equal? actual expected) #t (error "assertion failed")))