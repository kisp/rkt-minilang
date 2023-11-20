#lang typed/racket/base

(require typed/rackunit
         "parse-typed.rkt"
         "eval.rkt")

(define-syntax-rule (check-eval str exp)
  (check-equal? (value-of-program (parse-program! str)) exp))

(check-eval "1" 1)
(check-eval "123" 123)

(check-eval "-(10,2)" 8)
(check-eval "-(10,20)" -10)

(check-eval "zero?(0)" #t)
(check-eval "zero?(1)" #f)

(check-eval "if zero?(0) then 1 else 2" 1)
(check-eval "if zero?(1) then 1 else 2" 2)

(check-eval "let x = 1 in -(x,x)" 0)
(check-eval "let x = 100 in -(x,20)" 80)
(check-eval "let x = 1 in let x = 2 in x" 2)
(check-eval "let x = 1 in let y = 2 in x" 1)

(check-eval
 "
let x = 5
in let y = 5
in if zero?(-(x,y)) then
     y
   else
     -(x, 2)"
 5)

(check-eval
 "
let x = 5
in let y = 6
in if zero?(-(x,y)) then
     y
   else
     -(x, 2)"
 3)
