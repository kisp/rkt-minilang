#lang racket/base
(require racket/match
         megaparsack megaparsack/text
         data/monad data/applicative)

(struct const-exp (e1) #:transparent)
(struct diff-exp (e1 e2) #:transparent)
(struct if-exp (e1 e2 e3) #:transparent)

(define (full/p p)
  (do
    [x <- p]
    eof/p
    (pure x)))

(define (trail-ws/p p)
  (do
    [x <- p]
    (many/p space/p)
    (pure x)))

(define (tok/p s) (trail-ws/p (string/p s)))

(define const-exp/p
  (do
    [x <- integer/p]
    (pure (const-exp x))))

(define diff-exp/p
  (do
    (tok/p "-")
    (tok/p "(")
    [e1 <- exp/p]
    (tok/p ",")
    [e2 <- exp/p]
    (tok/p ")")
    (pure (diff-exp e1 e2))))

(define if-exp/p
  (do
    (tok/p "if")
    [e1 <- exp/p]
    (tok/p "then")
    [e2 <- exp/p]
    (tok/p "else")
    [e3 <- exp/p]
    (pure (if-exp e1 e2 e3))))

(define exp/p
  (trail-ws/p
   (or/p
    const-exp/p
    diff-exp/p
    if-exp/p)))

(define (parse-exp! s)
  (parse-result! (parse-string (full/p exp/p) s)))

(define (exp->exp-w-literals exp)
  (define (rec exp)
    (match exp
      [(const-exp x) x]
      [(diff-exp e1 e2) (diff-exp (rec e1) (rec e2))]
      [(if-exp e1 e2 e3) (if-exp (rec e1) (rec e2) (rec e3))]))
  (rec exp))

(define (parse-exp!-w-literals s)
  (exp->exp-w-literals (parse-exp! s)))

(module+ test
  (require rackcheck rackunit)

  (define (check-exp-parse? s e)
    (check-equal? (parse-exp!-w-literals s) e))

  (check-exp-parse? "if 1 then 2 else 3" (if-exp 1 2 3))
  (check-exp-parse? "if if 1 then 2 else 3 then 2 else 3" (if-exp (if-exp 1 2 3) 2 3))
  (check-exp-parse? "if1then2else3" (if-exp 1 2 3)) ;maybe not :)

  (check-exp-parse? "-(1, 2)" (diff-exp 1 2))
  (check-exp-parse? "-(1,2)" (diff-exp 1 2))
  (check-exp-parse? "- ( 1 , 2 )" (diff-exp 1 2))

  (check-exp-parse?
   "if if -
( 123
,
15
)
then
-(2,3)
else 2 then 2 else 1"
   (if-exp (if-exp (diff-exp 123 15) (diff-exp 2 3) 2) 2 1))

  ;; (check-property
  ;;  (property ([n (gen:integer-in 3 100)])
  ;;            (define numbers (stream->list (stream-take (fibs) n)))
  ;;            (for ([n (cddr numbers)]
  ;;                  [y (cdr numbers)]
  ;;                  [x numbers])
  ;;              (check-eqv? (+ x y) n))))
  )
