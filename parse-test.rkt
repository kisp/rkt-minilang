#lang racket/base

;; (require rackcheck)
(require racket/match
         rackunit
         "types.rkt"
         "parse.rkt")

(define-syntax-rule (check-exp-parse? s e)
  (check-equal? (parse-exp! s) e))

(define-syntax-rule (check-program-parse? s e)
  (check-equal? (parse-program! s) e))

(define (call-with-to-exp fn . args)
  (define (to-exp x)
    (match x
      [(? integer?) (const-exp x)]
      [(? symbol?) (var-exp x)]
      [_ x]))
  (apply fn (map to-exp args)))

(define (diff-exp* e1 e2)
  (call-with-to-exp diff-exp e1 e2))

(define (zero?-exp* e1)
  (call-with-to-exp zero?-exp e1))

(define (if-exp* e1 e2 e3)
  (call-with-to-exp if-exp e1 e2 e3))

(define (let-exp* e1 e2 e3)
  (call-with-to-exp let-exp e1 e2 e3))

(check-exp-parse? "1" (const-exp 1))
(check-exp-parse? "123" (const-exp 123))

(check-exp-parse? "-(1, 2)" (diff-exp* 1 2))
(check-exp-parse? "-(1,2)" (diff-exp* 1 2))
(check-exp-parse? "- ( 1 , 2 )" (diff-exp* 1 2))

(check-exp-parse? "zero?(1)" (zero?-exp* 1))
(check-exp-parse? "zero? ( 123 )" (zero?-exp* 123))

(check-exp-parse? "if 1 then 2 else 3" (if-exp* 1 2 3))
(check-exp-parse? "if if 1 then 2 else 3 then 2 else 3" (if-exp* (if-exp* 1 2 3) 2 3))
(check-exp-parse? "if1then2else3" (if-exp* 1 2 3)) ;maybe not :)

(check-exp-parse? "a" (var-exp 'a))
(check-exp-parse? "ab" (var-exp 'ab))
(check-exp-parse? "x1" (var-exp 'x1))

(check-exp-parse? "let x=1 in x" (let-exp* 'x 1 'x))
(check-exp-parse? "let x = 1 in -(x, 1)" (let-exp* 'x 1 (diff-exp* 'x 1)))
(check-exp-parse? "let x = -(y, 1) in x" (let-exp* 'x (diff-exp* 'y 1) 'x))

(check-exp-parse?
 "if if -
( 123
,
15
)
then
-(2,3)
else 2 then 2 else 1"
 (if-exp (if-exp (diff-exp* 123 15) (diff-exp* 2 3) (const-exp 2)) (const-exp 2) (const-exp 1)))

(check-program-parse? "-(55, -(x, 11))" (a-program (diff-exp* 55 (diff-exp* 'x 11))))
(check-program-parse? " -(55, -(x, 11))" (a-program (diff-exp* 55 (diff-exp* 'x 11))))

;; (check-property
;;  (property ([n (gen:integer-in 3 100)])
;;            (define numbers (stream->list (stream-take (fibs) n)))
;;            (for ([n (cddr numbers)]
;;                  [y (cdr numbers)]
;;                  [x numbers])
;;              (check-eqv? (+ x y) n))))
