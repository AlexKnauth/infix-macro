#lang racket/base

(provide add mult neg inv ex)

(require racket/stxparam
         (prefix-in rkt: racket/base)
         (for-syntax racket/base
                     ))

(define-syntax-parameter add (make-rename-transformer #'rkt:+))
(define-syntax-parameter mult (make-rename-transformer #'rkt:*))
(define-syntax-parameter neg (make-rename-transformer #'rkt:-))
(define-syntax-parameter inv (make-rename-transformer #'rkt:/))
(define-syntax-parameter ex (make-rename-transformer #'rkt:expt))

