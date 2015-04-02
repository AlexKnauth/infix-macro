#lang racket/base

(provide :)

(require "general-infix.rkt"
         (for-syntax racket/base
                     "general-infix-ct.rkt"
                     ))
(module+ test
  (require rackunit))

(module+ test
  (check-equal? (: 1 + 2) 3)
  (check-equal? (: 1 + 2 * 3) 7)
  (check-equal? (: 3 * 2 + 1) 7)
  (check-equal? (: 3 * 2 ^ 2 + 1) 13)
  (check-equal? (: 5 - 2) 3)
  (check-equal? (: 5 - 2 + 3) 6)
  (check-equal? (: - 5 + 7) 2)
  (check-equal? (: 6 / 2) 3)
  (check-equal? (: 6 / 2 * 3) 9)
  (check-equal? (: 6 / 2 ^ 2 * 3) 9/2)
  (define a 1)
  (check-equal? (: a + 1) 2)
  (check-equal? (: #:with 1 + 2 #:as a
                   #:let b = a + 4
                   #:let c = a + b #:in
                   a + b + c)
                20)
  (check-equal? (: sqrt 4 + 1) 3)
  (check-equal? (: sqrt 2 ^ 4 / 2) 2)
  (check-equal? (: sin cos 5) (sin (cos 5)))
  (check-equal? (: cos sin 5) (cos (sin 5)))
  (define-infix-macro/infix-parser ::
    (ops->parser (unary-prefix-op #:sym 'sqrt #:id #'sqrt) add-op expt-op))
  (check-equal? (: sqrt 3 ^ 2 + 4 ^ 2) 19)
  (check-equal? (:: sqrt 3 ^ 2 + 4 ^ 2) 5)
  )
