#lang racket/base

(require racket/stxparam
         syntax/parse/define
         (prefix-in rkt: racket/base)
         (for-syntax racket/base
                     ))

(define-simple-macro (defstxprmrnmrs [param:id rnm:id] ...)
  (begin (provide param ...) (define-syntax-parameter param (make-rename-transformer #'rnm)) ...))

(defstxprmrnmrs
  [add rkt:+]
  [mult rkt:*]
  [neg rkt:-]
  [inv rkt:/]
  [ex rkt:expt]
  [sqrrt rkt:sqrt]
  [sine rkt:sin]
  [cosine rkt:cos]
  [tang rkt:tan]
  [asine rkt:asin]
  [acosine rkt:acosine]
  [atang rkt:atan]
  [absval rkt:magnitude]
  [log_e rkt:log]
  )

