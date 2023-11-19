#lang typed/racket/base

(provide define-datatype)

(require (for-syntax racket/base))

(define-syntax define-datatype
  (lambda (stx)
    (syntax-case stx ()
      [(_ type-name . variants)
       #`(begin
           (define-type type-name
             (U #,@(for/list ([variant (syntax->list #'variants)])
                     (with-syntax ([(variant-name . fields) variant])
                       #'variant-name))))
           #,@(for/list ([variant (syntax->list #'variants)])
                (with-syntax ([(variant-name . fields) variant])
                  #`(struct variant-name
                      (#,@(for/list ([field (syntax->list #'fields)])
                            (with-syntax ([(field-name field-type) field])
                              #`[field-name : field-type])))
                      #:transparent))))])))
