#lang racket/base
(require (prefix-in r: readline/readline)
         racket/match racket/pretty
         data/either
         megaparsack
         "main.rkt")

(define (readline prompt)
  (if (member (getenv "TERM") '("dumb"))
      (begin
        (write-string "> ")
        (flush-output)
        (read-line))
      (let ([input (r:readline prompt)])
        (unless (eof-object? input)
          (r:add-history input))
        input)))

(define (repl)
  (let rec ()
    (let ([input (readline "> ")])
      (unless (or (eof-object? input)
                  (member input '("q" "quit" "exit")))
        (match (parse-program input)
          [(success ast) (pretty-print ast)]
          [(failure message) (displayln (parse-error->string message))])
        (rec)))))

(repl)
