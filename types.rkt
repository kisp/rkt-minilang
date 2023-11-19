#lang typed/racket/base

(require "define-datatype.rkt")

(provide Num Var)

(define-type Num Integer)
(define-type Var Symbol)

(define-datatype Exp
  (const-exp (e1 Num))
  (diff-exp (e1 Exp) (e2 Exp))
  (zero?-exp (e1 Exp))
  (if-exp (e1 Exp) (e2 Exp) (e3 Exp))
  (var-exp (e1 Var))
  (let-exp (id var-exp) (e1 Exp) (body Exp)))

(define-datatype Progr
  (a-program (e1 Exp)))
