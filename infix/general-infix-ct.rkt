#lang racket/base

(provide base-parser
         binary-op
         add/sub-like-op
         add-op
         mult-op
         expt-op
         let-op
         +-*/^-parser
         let+-*/^-parser
         infix-parser->transformer
         defstxcls/infix-parser
         defstxcls/op
         )

(require syntax/parse
         syntax/stx
         racket/bool
         syntax/parse/define
         (for-template racket/base
                       "syntax-parameters.rkt")
         (for-syntax racket/base
                     syntax/parse
                     ))

;; an Infix-Parser is a function that takes a stx-list and produces a syntax object or false
;; (define-type Infix-Parser [Stx-List -> (U Syntax #f)])

;; an Infix-Op is a function that takes an Infix-Parser and returns an augmented Infix-Parser
;; (define-type Infix-Op [Infix-Parser -> Infix-Parser])
;; the infix parser argument is the parser it should use for sub-expressions

(define (infix-parser->transformer p)
  (define (transformer stx)
    (orig (p (stx-cdr stx))))
  transformer)

;; base-parser : Infix-Parser
(define (base-parser stxlst)
  (syntax-parse stxlst
    [(a) #'a]))

(define-simple-macro (defstxcls/infix-parser stxcls-id:id p-expr:expr #:sub-pat sub-pat)
  #:with ooo (quote-syntax ...)
  (begin (define p p-expr)
         (define-splicing-syntax-class stxcls-id
           #:attributes (norm)
           [pattern (~seq (~and a sub-pat) ooo)
                    #:with stxlst #'(a ooo)
                    #:attr norm (with-handlers ([exn:fail:syntax? (λ (e) #f)])
                                  (orig (p #'stxlst)))
                    #:when (attribute norm)])))

(define-simple-macro (defstxcls/op stxcls-id:id #:sym sym-expr:expr #:attr attr-id #:id id-expr:expr)
  (begin (define sym sym-expr)
         (define/syntax-parse id-stx:id id-expr)
         (define-syntax-class stxcls-id #:attributes (attr-id)
           [pattern stx:id #:when (symbol=? (syntax-e #'stx) sym)
                    #:with attr-id (orig (syntax/loc #'stx id-stx))])))

(define (orig stx)
  (syntax-property stx 'original-for-check-syntax #t))
(define disappeared-use 'disappeared-use)
(define (disap stx . ids)
  (define stx.ids (or (syntax-property stx disappeared-use) '()))
  (orig (syntax-property stx disappeared-use
                         (map orig (append stx.ids (map syntax-local-introduce ids))))))

;; binary-op : [#:sym Sym #:id Id -> Infix-Op]
(define (binary-op #:sym sym #:id id-stx_)
  (defstxcls/op op-id #:sym sym #:attr op #:id id-stx_)
  ;; op : Infix-Op
  (define (op p)
    (defstxcls/infix-parser pexpr p #:sub-pat (~not :op-id))
    ;; parse : Infix-Parser
    (define (parse stxlst)
      (syntax-parse stxlst
        [(a:pexpr) #'a.norm]
        [(a:pexpr op:op-id b:pexpr)
         #'(op.op a.norm b.norm)]))
    parse)
  op)
    

;; add-op/sub-like-op : [#:add-sym Sym #:neg-sym Sym #:add-id Id #:neg-id Id -> Infix-Op]
(define (add/sub-like-op #:add-sym add-sym #:neg-sym neg-sym #:add-id add-id #:neg-id neg-id)
  (define/syntax-parse add add-id)
  (defstxcls/op + #:sym add-sym #:attr + #:id add-id)
  (defstxcls/op - #:sym neg-sym #:attr - #:id neg-id)
  ;; op : Infix-Op
  (define (op p)
    (defstxcls/infix-parser pexpr p #:sub-pat (~not (~or :+ :-)))
    (define-splicing-syntax-class +/-pexpr
      #:attributes (norm)
      [pattern (~seq +:+ a:pexpr) #:with norm (disap #'a.norm #'+.+)]
      [pattern (~seq -:- a:pexpr) #:with norm #'(-.- a.norm)])
    ;; parse : Infix-Parser
    (define (parse stxlst)
      (syntax-parse stxlst
        [((~or a:pexpr a:+/-pexpr)) #'a.norm]
        [((~or a:pexpr a:+/-pexpr) b:+/-pexpr ...)
         #'(add a.norm b.norm ...)]))
    parse)
  op)

;; add-op : Infix-Op
(define add-op (add/sub-like-op #:add-sym '+ #:neg-sym '- #:add-id #'add #:neg-id #'neg))

;; mult-op : Infix-Op
(define mult-op (add/sub-like-op #:add-sym '* #:neg-sym '/ #:add-id #'mult #:neg-id #'inv))

;; expt-op : Infix-Op
(define expt-op (binary-op #:sym '^ #:id #'ex))

;; +-*/^-parser : Infix-Parser
(define +-*/^-parser
  (add-op (mult-op (expt-op base-parser))))

;; let-op : Infix-Op
(define (let-op p)
  (defstxcls/infix-parser pexpr p #:sub-pat (~not (~or #:let (~datum =) #:with #:as)))
  (define-splicing-syntax-class let-expr #:attributes (norm) #:datum-literals (=)
    [pattern (~seq a:pexpr) #:with norm #'a.norm]
    [pattern (~seq #:let id:id = a:pexpr (~or (~seq #:in b:let-expr)
                                              (~and (~seq (~or #:let #:with) _ ...)
                                                    (~seq b:let-expr))))
             #:with norm #'(let ([id a.norm]) b.norm)]
    [pattern (~seq #:with a:pexpr #:as id:id (~optional #:in) b:let-expr)
             #:with norm #'(let ([id a.norm]) b.norm)])
  (define (parse stx)
    (syntax-parse stx
      [(a:let-expr) #'a.norm]))
  parse)

;; let+-*/^-parser : Infix-Parser
(define let+-*/^-parser
  (let-op +-*/^-parser))



