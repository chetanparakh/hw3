#lang racket
(require rackunit)
(require rackunit/text-ui)
(require "mceval.rkt")

(define (test-mc-eval exp)
  (with-handlers ([exn:fail? (lambda (exn) exn)])
    (mc-eval exp (setup-environment))))

(define (test-mc-eval-exception exp)
  (mc-eval exp (setup-environment)))

(define hw3-tests
  (test-suite
   "Homework 3 Tests"
   (test-case
    "Test primitives"
    (check-equal? (test-mc-eval '(+ 4 5))
                  9
                  "Implement +")
    (check-equal? (test-mc-eval '(* 4 5))
                  20
                  "Implement *")
    (check-equal? (test-mc-eval '(- 4 5))
                  -1
                  "Implement -")
    (check-equal? (test-mc-eval '(/ 8 4))
                  2
                  "Implement /")
    (check-equal? (test-mc-eval '(< 4 4))
                  #f
                  "Implement <")
    (check-equal? (test-mc-eval '(= 4 4))
                  #t
                  "Implement =")
    (check-equal? (test-mc-eval '(>= 4 4))
                  #t
                  "Implement >=")
    (check-equal? (test-mc-eval '(> 4 4))
                  #f
                  "Implement >")
    (check-exn (regexp "^Metacircular Interpreter Aborted$")
               (lambda () (test-mc-eval-exception '(error)))
               "Implement error"))
  
   (test-case
    "Test the and special form"
    (check-equal? (test-mc-eval '(and (= 2 2) (> 2 1)))
                  #t
                  "(and (= 2 2) (> 2 1))")
    (check-equal? (test-mc-eval '(and (= 2 2) (< 2 1)))
                  #f
                  "(= 2 2) (< 2 1))")
    (check-equal? (test-mc-eval '(and 1 2 'c '(f g)))
                  '(f g)
                  "(and 1 2 'c '(f g)))")
    (check-equal? (test-mc-eval '(and false (error)))
                  #f
                  "(and false (error))")
    (check-equal? (test-mc-eval '(and))
                  #t
                  "(and)"))

   (test-case
    "Test the or special form"
     (check-equal? (test-mc-eval '(or (= 2 2) (> 2 1)))
                   #t
                   "(or (= 2 2) (> 2 1))")
     (check-equal? (test-mc-eval '(or (= 2 2) (< 2 1)))
                   #t
                   "(or (= 2 2) (< 2 1))")
     (check-equal? (test-mc-eval '(or false false false))
                   #f
                   "(or false false false)")
     (check-equal? (test-mc-eval '(or true (error)))
                   #t
                   "(or true (error))")
     (check-equal? (test-mc-eval '(or))
                   #f
                   "(or)"))

   (test-case
    "Test the let special form"
    (check-equal? (test-mc-eval '(let ((x 1) (y 2)) (+ x y)))
                  3
                  "(let ((x 1) (y 2)) (+ x y))"))

   (test-case
    "Test delay and force"
    (check-equal? (test-mc-eval '(begin (delay (error)) 3))
                  3
                  "(begin (delay (error)) 3)")
    (check-equal? (test-mc-eval '(force (delay 3)))
                  3
                  "(force (delay 3))")
    (check-equal? (test-mc-eval '((delay 5)))
                  5
                  "((delay 5))")
    (check-equal? (test-mc-eval '(let ((x (delay 3))) (force x)))
                  3
                  "(let ((x (delay 3))) (force x))")
    (check-equal? (test-mc-eval '(force (delay (force (delay 3)))))
                  3
                  "(force (delay (force (delay 3))))")
    (check-equal? (test-mc-eval '(let ((x (delay (+ 1 2)))) (+ (force x) (force x))))
                  6
                  "(let ((x (delay (+ 1 2)))) (+ (force x) (force x)))")
    (check-equal? (test-mc-eval '(let ((x 0))
                                   (let ((y (delay (begin (set! x (+ x 1)) x))))
                                     (+ (force y) (force y)))))
                  2
                  "Delayed expression with side-effect"))

   (test-case
    "Test streams"
    (test-case
     "empty-stream"
     (check-equal? (test-mc-eval '(stream-empty? empty-stream))
                   #t
                   "(stream-empty? empty-stream)"))
    (test-case
     "stream-first and stream-cons"
     (check-equal? (test-mc-eval '(stream-first (stream-cons 1 empty-stream)))
                   1
                   "(stream-first (stream-cons 1 empty-stream))"))
    (test-case
     "stream-rest"
     (check-equal? (test-mc-eval '(stream-empty? (stream-rest (stream-cons 1 empty-stream))))
                   #t
                   "(stream-empty? (stream-rest (stream-cons 1 empty-stream)))"))
    (test-case
     "stream laziness"
     (check-equal? (test-mc-eval '(stream-first (stream-cons 1 (error))))
                   1
                   "(stream-first (stream-cons 1 (error)))")
     (check-equal? (test-mc-eval '(stream-first (stream-cons (+ 2 3) (stream-cons (+ 2 3) (error)))))
                   5
                   "(stream-first (stream-cons (+ 2 3) (stream-cons (+ 2 3) (error))))")
     (check-equal? (test-mc-eval '(stream-first (stream-rest (stream-cons (+ 2 3) (stream-cons (+ 2 3) (error))))))
                   5
                   "(stream-first (stream-rest (stream-cons (+ 2 3) (stream-cons (+ 2 3) (error)))))")))))

(when (not (eq? (run-tests hw3-tests) 0))
  (exit 1))
