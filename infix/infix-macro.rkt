#lang racket/base

(provide :)

(require "general-infix.rkt")
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
  )
