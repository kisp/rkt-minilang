#lang racket/base

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

(define-prod const-exp ([e1 num]))
(define-prod diff-exp ("-" "(" [e1 exp] "," [e2 exp] ")"))
(define-prod zero?-exp ("zero?" "(" [e1 exp] ")"))
(define-prod if-exp ("if" [e1 exp] "then" [e2 exp] "else" [e3 exp]))

(define exp/p
  (trail-ws/p
   (or/p
    const-exp/p
    diff-exp/p
    zero?-exp/p
    if-exp/p)))

(define (parse-exp! s)
  (parse-result! (parse-string (full/p exp/p) s)))

(define (exp->exp-w-literals exp)
  (define (rec exp)
    (match exp
      [(const-exp x) x]
      [(diff-exp e1 e2) (diff-exp (rec e1) (rec e2))]
      [(zero?-exp e1) (zero?-exp (rec e1))]
      [(if-exp e1 e2 e3) (if-exp (rec e1) (rec e2) (rec e3))]))
  (rec exp))

(define (parse-exp!-w-literals s)
  (exp->exp-w-literals (parse-exp! s)))

(module+ test
  (require rackcheck rackunit)

  (define (check-exp-parse? s e)
    (check-equal? (parse-exp!-w-literals s) e))

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
