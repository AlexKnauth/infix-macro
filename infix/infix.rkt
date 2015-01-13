#lang racket/base

(provide : + * - / ^)

(require racket/stxparam
         (prefix-in rkt: racket/base)
         (for-syntax (except-in racket/base + * - /)
                     syntax/parse))
(module+ test
  (require rackunit))

(define-syntax-parameter + (make-rename-transformer #'rkt:+))
(define-syntax-parameter * (make-rename-transformer #'rkt:*))
(define-syntax-parameter - (make-rename-transformer #'rkt:-))
(define-syntax-parameter / (make-rename-transformer #'rkt:/))
(define-syntax-parameter ^ (make-rename-transformer #'rkt:expt))

(begin-for-syntax
  (define-syntax-class op #:literals (+ * - / ^)
    [pattern (~or + * - / ^)])
  (define-syntax-class mexpr
    [pattern (~and norm:expr (~not :op))])
  (define-splicing-syntax-class sum
    [pattern (~seq (~or a:product a:+-product))
             #:with norm #'a.norm]
    [pattern (~seq (~or a:product a:+-product) b:+-product ...)
             #:with norm #'(+ a.norm b.norm ...)])
  (define-splicing-syntax-class product
    [pattern (~seq (~or a:mexpt a:*/expt))
             #:with norm #'a.norm]
    [pattern (~seq (~or a:mexpt a:*/expt) b:*/expt ...)
             #:with norm #'(* a.norm b.norm ...)])
  (define-splicing-syntax-class mexpt #:literals (^)
    [pattern (~seq a:mexpr)
             #:with norm #'a.norm]
    [pattern (~seq a:mexpr ^ b:mexpr)
             #:with norm #'(^ a.norm b.norm)])
  (define-splicing-syntax-class +-product #:literals (+ -)
    [pattern (~seq + a:product)
             #:with norm #'a.norm]
    [pattern (~seq - a:product)
             #:with norm #'(- a.norm)])
  (define-splicing-syntax-class */expt #:literals (* /)
    [pattern (~seq * a:mexpt)
             #:with norm #'a.norm]
    [pattern (~seq / a:mexpt)
             #:with norm #'(/ a.norm)])
  )
(define-syntax :
  (syntax-parser
    [(_ a:sum) #'a.norm]))

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
  )
