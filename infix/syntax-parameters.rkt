#lang racket/base

(provide add mult neg inv ex)

(require racket/stxparam
         syntax/parse/define
         (prefix-in rkt: racket/base)
         (for-syntax racket/base
                     ))

(define-simple-macro (defstxprmrnmrs [param:id rnm:id] ...)
  (begin (define-syntax-parameter param (make-rename-transformer #'rnm)) ...))

(defstxprmrnmrs
  [add rkt:+]
  [mult rkt:*]
  [neg rkt:-]
  [inv rkt:/]
  [ex rkt:expt]
  )

