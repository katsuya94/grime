(import (grime))

(define (assert-true condition)
  (unless condition (error "assertion failed")))

(define (assert-equal actual expected)
  (assert-true (equal? actual expected)))

(define-syntax unhygienic
  (lambda (stx)
    (syntax-case stx ()
      [(k e)
       (with-syntax [(y (datum->syntax #'k 'y))]
         #'(let [(y 'dirty)] e))])))

(define-syntax syntax-rules-test-simple
  (syntax-rules ()
    [(_) 'foo]))

(define-syntax with-marked-id
  (lambda (stx)
    (syntax-case stx ()
      [(_ e) #'(let [(id #f)] e])))

(define-syntax marked-id
  (lambda (stx)
    (syntax-case stx ()
      [(_) #'#'id])))

(define-syntax syntax-case-test-hygienic-literals
  (lambda (stx)
    (with-syntax
      [(unmarked #'id)
       (marked (marked-id))]
      ; in the patterns, unmarked is unambiguous because it is not captured by marked
      ; however, marked is ambiguous because it is captured by both marked and unmarked
      ; it fails with an "already defined" when we put them in the same scope
      ; typically a marked identifier does not share its scope with unmarked identifiers
      ; notably, reversing the order changes the issue
      ; questions:
      ; can this situation arise without literals? intuition: yes
      ; is this desirable behavior? intuition: no
      ; what does racket do?
      ; does it make sense for the order of literals to matter?
      #'(syntax-case #'(unmarked . marked) (unmarked marked)
          [(marked . unmarked) #f]
          [(unmarked . marked) #t]))))


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

; datum->syntax

(unhygienic
  (assert-equal y 'dirty))

; syntax-rules

(assert-equal (syntax-rules-test-simple) 'foo)

; syntax-case

(assert-true (let [(id #t)] (with-marked-id id)))

(assert-true (syntax-case-test-hygienic-literals))