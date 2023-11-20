repl:
	racket repl.rkt

main-repl:
	racket -ie '(enter! "repl.rkt")'

test:
	raco test *-test.rkt

compile:
	raco make -v *.rkt

clean:
	rm -rf compiled
