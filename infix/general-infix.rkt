#lang racket/base

(provide :
         define-infix-macro/infix-parser
         )

(require syntax/parse/define
         (for-syntax racket/base
                     syntax/parse
                     "general-infix-ct.rkt"
                     ))

(define-simple-macro (define-infix-macro/infix-parser macro-id:id parser-expr:expr)
  (define-syntax macro-id (infix-parser->transformer parser-expr)))

(define-infix-macro/infix-parser : let+-*/^-parser)

