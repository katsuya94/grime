(import (grime))

(define (assert-true condition)
  (unless condition (error "assertion failed")))

(define (assert-equal actual expected)
  (assert-true (equal? actual expected)))


(define x #f)

; when

(set! x #f)
(when #t (set! x #t))
(assert-equal x #t)

(set! x #f)
(when #f (set! x #t))
(assert-equal x #f)

; unless

(set! x #f)
(unless #t (set! x #t))
(assert-equal x #f)

(set! x #f)
(unless #f (set! x #t))
(assert-equal x #t)

; let*

(let* [(x 'foo)]
  (assert-equal x 'foo))

(let* [(x 'foo) (y 'bar)]
  (assert-equal x 'foo)
  (assert-equal y 'bar))

(let* [(x 'foo) (y x)]
  (assert-equal x 'foo)
  (assert-equal y 'foo))

; letrec*

(set! x #f)
(letrec* [(x 'foo) (y (lambda () x))]
  (assert-equal (y) 'foo))

; let

(let [(x 'foo)]
  (assert-equal x 'foo))

(let [(x 'foo) (y 'bar)]
  (assert-equal x 'foo)
  (assert-equal y 'bar))

(set! x #f)
(let [(x 'foo) (y x)]
  (assert-equal x 'foo)
  (assert-equal y #f))