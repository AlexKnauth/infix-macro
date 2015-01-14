#lang racket/base

(provide : add mult neg inv ex)

(require racket/stxparam
         (prefix-in rkt: racket/base)
         (for-syntax (except-in racket/base + * - /)
                     syntax/parse
                     syntax/parse/define
                     (for-syntax racket/base)))
(module+ test
  (require rackunit))

(define-syntax-parameter add (make-rename-transformer #'rkt:+))
(define-syntax-parameter mult (make-rename-transformer #'rkt:*))
(define-syntax-parameter neg (make-rename-transformer #'rkt:-))
(define-syntax-parameter inv (make-rename-transformer #'rkt:/))
(define-syntax-parameter ex (make-rename-transformer #'rkt:expt))

(begin-for-syntax
  (define (orig stx)
    (syntax-property stx 'original-for-check-syntax #t))
  (define disappeared-use 'disappeared-use)
  (define (disap stx . ids)
    (define stx.ids (or (syntax-property stx disappeared-use) '()))
    (syntax-property stx disappeared-use (append stx.ids (map syntax-local-introduce ids))))
  (define-simple-macro (define-op-syntax-class id:id stxparam:id)
    (define-syntax-class id
      #:attributes (id)
      [pattern (~and stx (~datum id)) #:with id (orig (syntax/loc #'stx stxparam))]))
  (define-op-syntax-class + add)
  (define-op-syntax-class * mult)
  (define-op-syntax-class - neg)
  (define-op-syntax-class / inv)
  (define-op-syntax-class ^ ex)
  (define-syntax-class op
    [pattern (~or :+ :* :- :/ :^)])
  (define-syntax-class mexpr
    [pattern (~and norm:expr (~not :op))])
  (define-splicing-syntax-class sum
    [pattern (~seq (~or a:product a:+-product))
             #:with norm #'a.norm]
    [pattern (~seq (~or a:product a:+-product) b:+-product ...)
             #:with norm #'(add a.norm b.norm ...)])
  (define-splicing-syntax-class product
    [pattern (~seq (~or a:mexpt a:*/expt))
             #:with norm #'a.norm]
    [pattern (~seq (~or a:mexpt a:*/expt) b:*/expt ...)
             #:with norm #'(mult a.norm b.norm ...)])
  (define-splicing-syntax-class mexpt
    [pattern (~seq a:mexpr)
             #:with norm #'a.norm]
    [pattern (~seq a:mexpr ^:^ b:mexpr)
             #:with norm #'(^.^ a.norm b.norm)])
  (define-splicing-syntax-class +-product
    [pattern (~seq +:+ a:product)
             #:with norm (disap #'a.norm #'+.+)]
    [pattern (~seq -:- a:product)
             #:with norm #'(-.- a.norm)])
  (define-splicing-syntax-class */expt
    [pattern (~seq *:* a:mexpt)
             #:with norm (disap #'a.norm #'*.*)]
    [pattern (~seq /:/ a:mexpt)
             #:with norm #'(/./ a.norm)])
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
