#lang typed/racket/base

(require racket/match
         typed/rackunit
         racket/set
         "define-datatype.rkt")

(define-datatype Lc-Exp
  (var-exp (var Symbol))
  (lambda-exp
   (bound-var Symbol)
   (body Lc-Exp))
  (app-exp
   (rator Lc-Exp)
   (rand Lc-Exp)))

(: free-vars (-> Lc-Exp (Setof Symbol)))
(define (free-vars e)
  (let rec ([e e]
            [env (set)])
    (match e
      [(var-exp id) (if (set-member? env id)
                        (set)
                        (set id))]
      [(lambda-exp id e) (rec e (set-add env id))]
      [(app-exp e1 e2) (set-union (rec e1 env) (rec e2 env))])))

(check-equal? (free-vars (var-exp 'foo)) (set 'foo))
(check-equal? (free-vars (lambda-exp 'x (var-exp 'foo))) (set 'foo))
(check-equal? (free-vars (lambda-exp 'foo (var-exp 'foo))) (set))
(check-equal? (free-vars (lambda-exp 'x (lambda-exp 'y (var-exp 'foo)))) (set 'foo))
(check-equal? (free-vars (lambda-exp 'foo (lambda-exp 'y (var-exp 'foo)))) (set))
(check-equal? (free-vars (app-exp
                          (lambda-exp 'x (var-exp 'x))
                          (lambda-exp 'y (var-exp 'foo))))
              (set 'foo))
(check-equal? (free-vars (app-exp
                          (lambda-exp 'x (var-exp 'bar))
                          (lambda-exp 'y (var-exp 'foo))))
              (set 'foo 'bar))
(check-equal? (free-vars (lambda-exp 'bar
                                     (app-exp
                                      (lambda-exp 'x (var-exp 'bar))
                                      (lambda-exp 'y (var-exp 'foo)))))
              (set 'foo))
(check-equal? (free-vars (app-exp
                          (app-exp
                           (lambda-exp 'x (var-exp 'a))
                           (lambda-exp 'y (var-exp 'b)))
                          (app-exp
                           (lambda-exp 'x (var-exp 'c))
                           (lambda-exp 'y (var-exp 'd)))))
              (set 'a 'b 'c 'd))
