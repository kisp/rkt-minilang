#lang typed/racket

(provide value-of value-of-program)

(require "types.rkt")
;; (require "parse-typed.rkt")

(: empty-env (-> Env))
(define (empty-env) '())

(: extend-env (-> Var ExpVal Env Env))
(define (extend-env x v env)
  (cons (cons x v) env))

(: lookup-env (-> Var Env ExpVal))
(define (lookup-env x env)
  (cdr
   (or (assoc x env)
       (error "Unbound variable" x))))

(: expval->num (-> ExpVal Integer))
(define (expval->num v)
  (if (integer? v)
      v
      (error "Not a number:" v)))

(: expval->bool (-> ExpVal Boolean))
(define (expval->bool v)
  (if (boolean? v)
      v
      (error "Not a bool:" v)))

(: num-val (-> Integer ExpVal))
(define (num-val x) x)

(: bool-val (-> Boolean ExpVal))
(define (bool-val x) x)

(: value-of (-> Exp Env ExpVal))
(define (value-of e env)
  (match e
    [(const-exp num) num]
    [(var-exp var) (lookup-env var env)]
    [(diff-exp e1 e2)
     (num-val
      (- (expval->num (value-of e1 env))
         (expval->num (value-of e2 env))))]
    [(zero?-exp e1)
     (bool-val (zero? (expval->num (value-of e1 env))))]
    [(if-exp e1 e2 e3)
     (if (expval->bool (value-of e1 env))
         (value-of e2 env)
         (value-of e3 env))]
    [(let-exp var e1 body)
     (value-of body
               (extend-env var (value-of e1 env) env))]))

(: value-of-program (-> Progr ExpVal))
(define (value-of-program p)
  (match p
    [(a-program e) (value-of e (empty-env))]))
