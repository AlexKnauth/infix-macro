infix-macro [![Build Status](https://travis-ci.org/AlexKnauth/infix-macro.png?branch=master)](https://travis-ci.org/AlexKnauth/infix-macro)
===

a macro for infix math operations

```racket
> (: 1 + 2)
3
> (: 1 + 2 * 3)
7
> (: sqrt 2 ^ 4 / 2) ; equivalent to (/ (sqrt (expt 2 4)) 2)
2
> (define-infix-macro/infix-parser ::
    (ops->parser (unary-prefix-op #:sym 'sqrt #:id #'sqrt) add-op expt-op))
> (: sqrt 3 ^ 2 + 4 ^ 2) ; equivalent to (+ (sqrt (expt 3 2)) (expt 4 2))
19
> (:: sqrt 3 ^ 2 + 4 ^ 2) ; equivalent to (sqrt (+ (expt 3 2) (expt 4 2)))
5
```
