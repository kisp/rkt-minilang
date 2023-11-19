#lang racket/base

(provide parse-program parse-program!
         parse-exp parse-exp!)

(require (for-syntax racket/base racket/syntax))
(require racket/match
         megaparsack megaparsack/text
         data/monad data/applicative
         "types.rkt")

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
  (parse-result! (parse-exp s)))

(define (parse-exp s)
  (parse-string (full/p exp/p) s))

(define (parse-program! s)
  (parse-result! (parse-program s)))

(define (parse-program s)
  (parse-string (full/p (leading-ws/p a-program/p)) s))
