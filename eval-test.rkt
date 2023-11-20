#lang typed/racket/base

(require typed/rackunit
         "types.rkt"
         "parse-typed.rkt"
         "eval.rkt")

(check-equal? (value-of (parse-exp! "1")) 1)
(check-equal? (value-of (parse-exp! "123")) 123)
(check-equal? (value-of (parse-exp! "-(10,2)")) 8)

(check-equal? (value-of-program (parse-program! "123")) 123)
