#lang typed/racket

(provide value-of value-of-program)

(require "types.rkt")

(: value-of (-> Exp Integer))
(define (value-of e)
  (match e
    [(const-exp x) x]
    [(diff-exp e1 e2) (- (value-of e1) (value-of e2))]))

(: value-of-program (-> Progr Integer))
(define (value-of-program p)
  (match p
    [(a-program e) (value-of e)]))
