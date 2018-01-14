#lang racket
(require rackunit)
(require rackunit/text-ui)
(require "lazy-mceval.rkt")

(define (test-mc-eval exp)
  (with-handlers ([exn:fail? (lambda (exn) exn)])
    (mc-eval exp (setup-environment))))

(define (test-mc-eval-sequence exps)
  (let ((env (setup-environment)))
    (define (loop exps)
      (if (null? (cdr exps))
          (mc-eval (car exps) env)
          (begin (mc-eval (car exps) env)
                 (loop (cdr exps)))))
    (loop exps)))

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
    (check-equal? (test-mc-eval '(and ((lambda (x) x) (= 2 2)) ((lambda (x) x) (> 2 1))))
                  #t
                  "(and ((lambda (x) x) (= 2 2)) ((lambda (x) x) (> 2 1)))")
    (check-equal? (test-mc-eval '(and ((lambda (x) x) (= 2 2)) ((lambda (x) x) (< 2 1))))
                  #f
                  "(and ((lambda (x) x) (= 2 2)) ((lambda (x) x) (< 2 1)))")
    (check-equal? (test-mc-eval '(and 1 2 'c '(f g)))
                  '(f g)
                  "(and 1 2 'c '(f g)))")
    (check-equal? (test-mc-eval '(and ((lambda (x) x) false) (error)))
                  #f
                  "(and ((lambda (x) x) false) (error))")
    (check-equal? (test-mc-eval '(and))
                  #t
                  "(and)"))

   (test-case
    "Test the or special form"
     (check-equal? (test-mc-eval '(or ((lambda (x) x) (= 2 2)) ((lambda (x) x) (> 2 1))))
                   #t
                   "(or ((lambda (x) x) (= 2 2)) ((lambda (x) x) (> 2 1)))")
     (check-equal? (test-mc-eval '(or ((lambda (x) x) (= 2 2)) ((lambda (x) x) (< 2 1))))
                   #t
                   "(or ((lambda (x) x) (= 2 2)) ((lambda (x) x) (< 2 1)))")
     (check-equal? (test-mc-eval '(or false false false))
                   #f
                   "(or false false false)")
     (check-equal? (test-mc-eval '(or ((lambda (x) x) true) (error)))
                   #t
                   "(or ((lambda (x) x) true) (error))")
     (check-equal? (test-mc-eval '(or))
                   #f
                   "(or)"))

   (test-case
    "Test hybrid evaluator"
    (check-exn (regexp "^/: division by zero$")
               (lambda () (test-mc-eval-sequence '((define (try a b) (if (= a 0) 1 b)) (try 0 (/ 1 0)))))
               "Applicatiuve-order evaluation")
    (check-equal? (test-mc-eval-sequence '((define (try a (delay b)) (if (= a 0) 1 b)) (try 0 (/ 1 0))))
                  1
                  "Normal-order evaluation"))))

(when (not (eq? (run-tests hw3-tests) 0))
  (exit 1))
