#lang racket/base

(provide parse-program parse-program!)

(require (for-syntax racket/base racket/syntax))
(require racket/match
         megaparsack megaparsack/text
         data/monad data/applicative)

(define-syntax (define-prod stx)
  (syntax-case stx ()
    [(_ name (fields ...))
     (with-syntax ([name/p (format-id #'name "~a/p" #'name)]
                   [field-names (for/list ([field (syntax->datum #'(fields ...))]
                                           #:unless (string? field))
                                  (car field))]
                   [parsers (for/list ([field (syntax->list #'(fields ...))])
                              (if (string? (syntax-e field))
                                  field
                                  (syntax-case field ()
                                    [(_ parser)
                                     (with-syntax ([parser/p (format-id #'parser "~a/p" #'parser)])
                                       #'(delay/p parser/p))])))])
       #`(begin
           (struct name field-names #:transparent)
           (define name/p
             (parse-and-apply/p
              name
              #,@(for/list ([field (syntax->list #'(fields ...))])
                   (if (string? (syntax-e field))
                       field
                       (syntax-case field ()
                         [(_ parser)
                          (with-syntax ([parser/p (format-id #'parser "~a/p" #'parser)])
                            #'(delay/p parser/p))])))))))]))

(define (full/p p)
  (do
    [x <- p]
    eof/p
    (pure x)))

(define (leading-ws/p p)
  (do
    (many/p space/p)
    p))

(define (trail-ws/p p)
  (do
    [x <- p]
    (many/p space/p)
    (pure x)))

(define (tok/p s) (trail-ws/p (string/p s)))

(define (parse-and-apply/p c . lst)
  (define (rec lst acc)
    (match lst
      [(list) (pure (apply c (reverse acc)))]
      [(cons (? string? a) b)
       (chain
        (lambda (_)
          (rec b acc))
        (tok/p a))]
      [(cons a b)
       (chain
        (lambda (s)
          (rec b (cons s acc)))
        a)]))
  (rec lst '()))

(define num/p integer/p)
(define var/p
  (trail-ws/p
   (do
     [c <- letter/p]
     [cs <- (many/p (or/p letter/p digit/p))]
     (pure (string->symbol (list->string (cons c cs)))))))

(define-prod const-exp ([e1 num]))
(define-prod diff-exp ("-" "(" [e1 exp] "," [e2 exp] ")"))
(define-prod zero?-exp ("zero?" "(" [e1 exp] ")"))
(define-prod if-exp ("if" [e1 exp] "then" [e2 exp] "else" [e3 exp]))
(define-prod var-exp ([e1 var]))
(define-prod let-exp ("let" [id var-exp] "=" [e1 exp] "in" [body exp]))

(define exp/p
  (trail-ws/p
   (or/p
    const-exp/p
    diff-exp/p
    zero?-exp/p
    if-exp/p
    let-exp/p
    var-exp/p)))

(define-prod a-program ([e1 exp]))

(define (parse-exp! s)
  (parse-result! (parse-string (full/p exp/p) s)))

(define (parse-program! s)
  (parse-result! (parse-program s)))

(define (parse-program s)
  (parse-string (full/p (leading-ws/p a-program/p)) s))

(define (exp->exp-w-literals exp)
  (define (rec exp)
    (match exp
      [(const-exp x) x]
      [(diff-exp e1 e2) (diff-exp (rec e1) (rec e2))]
      [(zero?-exp e1) (zero?-exp (rec e1))]
      [(if-exp e1 e2 e3) (if-exp (rec e1) (rec e2) (rec e3))]
      [(var-exp e1) e1]
      [(let-exp id e1 body) (let-exp (rec id) (rec e1) (rec body))]))
  (rec exp))

(define (program->program-w-literals program)
  (match program
    [(a-program e1) (a-program (exp->exp-w-literals e1))]))

(define (parse-exp!-w-literals s)
  (exp->exp-w-literals (parse-exp! s)))

(define (parse-program!-w-literals s)
  (program->program-w-literals (parse-program! s)))

(module+ test
  (require rackcheck rackunit)

  (define-syntax-rule (check-exp-parse? s e)
    (check-equal? (parse-exp!-w-literals s) e))

  (define-syntax-rule (check-program-parse? s e)
    (check-equal? (parse-program!-w-literals s) e))

  (check-exp-parse? "1" 1)
  (check-exp-parse? "123" 123)

  (check-exp-parse? "-(1, 2)" (diff-exp 1 2))
  (check-exp-parse? "-(1,2)" (diff-exp 1 2))
  (check-exp-parse? "- ( 1 , 2 )" (diff-exp 1 2))

  (check-exp-parse? "zero?(1)" (zero?-exp 1))
  (check-exp-parse? "zero? ( 123 )" (zero?-exp 123))

  (check-exp-parse? "if 1 then 2 else 3" (if-exp 1 2 3))
  (check-exp-parse? "if if 1 then 2 else 3 then 2 else 3" (if-exp (if-exp 1 2 3) 2 3))
  (check-exp-parse? "if1then2else3" (if-exp 1 2 3)) ;maybe not :)

  (check-exp-parse? "a" 'a)
  (check-exp-parse? "ab" 'ab)
  (check-exp-parse? "x1" 'x1)

  (check-exp-parse? "let x=1 in x" (let-exp 'x 1 'x))
  (check-exp-parse? "let x = 1 in -(x, 1)" (let-exp 'x 1 (diff-exp 'x 1)))
  (check-exp-parse? "let x = -(y, 1) in x" (let-exp 'x (diff-exp 'y 1) 'x))

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

  (check-program-parse? "-(55, -(x, 11))" (a-program (diff-exp 55 (diff-exp 'x 11))))
  (check-program-parse? " -(55, -(x, 11))" (a-program (diff-exp 55 (diff-exp 'x 11))))

  ;; (check-property
  ;;  (property ([n (gen:integer-in 3 100)])
  ;;            (define numbers (stream->list (stream-take (fibs) n)))
  ;;            (for ([n (cddr numbers)]
  ;;                  [y (cdr numbers)]
  ;;                  [x numbers])
  ;;              (check-eqv? (+ x y) n))))
  )
