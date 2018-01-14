RACOEXEFLAGS=--collects-path $(shell readlink -m $$(which racket)/../../share/racket/collects) --exf-clear ++exf -m ++exf -U ++exf --

.PHONY : all
all : mceval lazy-mceval

.PHONY : clean
clean :
	rm -rf mceval lazy-mceval test-mceval test-lazy-mceval compiled

mceval : mceval.rkt
	raco exe $(RACOEXEFLAGS) -o $@ $<

lazy-mceval : lazy-mceval.rkt
	raco exe $(RACOEXEFLAGS) -o $@ $<

test-mceval : test-mceval.rkt mceval.rkt
	raco exe -o $@ $<

test-lazy-mceval : test-lazy-mceval.rkt lazy-mceval.rkt
	raco exe -o $@ $<

.PHONY : test
test : test-mceval test-lazy-mceval
	./test-mceval
	./test-lazy-mceval
