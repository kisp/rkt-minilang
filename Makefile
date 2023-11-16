repl:
	racket repl.rkt

main-repl:
	racket -ie '(enter! "main.rkt")'

test:
	raco test main.rkt
