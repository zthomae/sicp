#lang sicp

(#%require "amb.rkt"
         rackunit
         rackunit/text-ui
         mock
         mock/rackunit
         (prefix racket: racket))

(define (with-amb-mocks f)
  (let ((succeed-mock (mock #:behavior racket:void))
        (fail-mock (mock #:behavior racket:void)))
    (f succeed-mock fail-mock)))

(define (check-mock-result mock result)
  (let* ((calls (mock-calls mock))
         (first-arguments (arguments-positional (mock-call-args (racket:car calls)))))
    (check-equal? (racket:car first-arguments) result)))

(define tests
  (test-suite
   "Amb interpreter tests"

   (test-suite
    "self evaluating"
    (test-case "evaluates to self -- number"
               (with-amb-mocks
                   (lambda (succeed-mock fail-mock)
                     (ambeval 1 (setup-environment) succeed-mock fail-mock)
                     (check-mock-result succeed-mock 1))))
    (test-case "evaluates to self -- string"
               (with-amb-mocks
                   (lambda (succeed-mock fail-mock)
                     (ambeval "asdf" (setup-environment) succeed-mock fail-mock)
                     (check-mock-result succeed-mock "asdf"))))
    (test-case "does not call fail with success value"
               (with-amb-mocks
                   (lambda (succeed-mock fail-mock)
                     (ambeval 1 (setup-environment) succeed-mock fail-mock)
                     (check-mock-calls fail-mock racket:null)))))

   (test-suite
    "quoted"
    (test-case "evaluates to quoted text"
               (with-amb-mocks
                   (lambda (succeed-mock fail-mock)
                     (ambeval (quote 'a) (setup-environment) succeed-mock fail-mock)
                     (check-mock-result succeed-mock 'a))))
    (test-case "does not call fail"
               (with-amb-mocks
                   (lambda (succeed-mock fail-mock)
                     (ambeval (quote 'a) (setup-environment) succeed-mock fail-mock)
                     (check-mock-calls fail-mock racket:null)))))

   (test-suite
    "variable"
    (test-case "evaluates to variable value"
               (with-amb-mocks
                   (lambda (succeed-mock fail-mock)
                     (let ((env '(((a) 1))))
                       (ambeval 'a env succeed-mock fail-mock)
                       (check-mock-result succeed-mock 1)))))
    (test-case "does not call fail"
               (with-amb-mocks
                   (lambda (succeed-mock fail-mock)
                     (let ((env '(((a) 1))))
                       (ambeval 'a env succeed-mock fail-mock)
                       (check-mock-calls fail-mock racket:null))))))

   (test-suite
    "amb"
    (test-case "does not call succeed if there are no choices"
               (with-amb-mocks
                   (lambda (succeed-mock fail-mock)
                     (ambeval '(amb) (setup-environment) succeed-mock fail-mock)
                     (check-mock-calls succeed-mock racket:null))))
    (test-case "calls fail mock if there are no choices"
               (with-amb-mocks
                   (lambda (succeed-mock fail-mock)
                     (ambeval '(amb) (setup-environment) succeed-mock fail-mock)
                     (check-mock-calls fail-mock (racket:list empty-arguments)))))
    (test-case "calls succeed with value of first choice after one step of evaluation"
               (with-amb-mocks
                   (lambda (succeed-mock fail-mock)
                     (ambeval '(amb 1) (setup-environment) succeed-mock fail-mock)
                     (check-mock-result succeed-mock 1))))
    (test-case "does not call fail if first choice fails"
               (with-amb-mocks
                   (lambda (succeed-mock fail-mock)
                     (ambeval '(amb (amb) 1) (setup-environment) succeed-mock fail-mock)
                     (check-mock-calls fail-mock racket:null))))
    (test-case "calls fail if both choices fail"
               (with-amb-mocks
                   (lambda (succeed-mock fail-mock)
                     (ambeval '(amb (amb) (amb)) (setup-environment) succeed-mock fail-mock)
                     (check-mock-calls fail-mock (racket:list empty-arguments)))))
    (test-case "calls succeed with value of second choice after first choice fails"
               (with-amb-mocks
                   (lambda (succeed-mock fail-mock)
                     (ambeval '(amb (amb) 1) (setup-environment) succeed-mock fail-mock)
                     (check-mock-result succeed-mock 1)))))
      
   (test-suite
    "begin"
    (test-case "fails if the sequence is empty"
               (with-amb-mocks
                   (lambda (succeed-mock fail-mock)
                     (check-exn racket:exn:fail? (lambda () (ambeval '(begin) (setup-environment) succeed-mock fail-mock))))))
    (test-case "calls succeed with last value in sequence"
               (with-amb-mocks
                   (lambda (succeed-mock fail-mock)
                     (ambeval '(begin 1 2) (setup-environment) succeed-mock fail-mock)
                     (check-mock-result succeed-mock 2))))
    (test-case "does not call fail if no sequence evaluations fail"
               (with-amb-mocks
                   (lambda (succeed-mock fail-mock)
                     (ambeval '(begin 1 2) (setup-environment) succeed-mock fail-mock)
                     (check-mock-calls fail-mock racket:null))))
    (test-case "calls fail if first in sequence fails"
               (with-amb-mocks
                   (lambda (succeed-mock fail-mock)
                     (ambeval '(begin (amb) 1) (setup-environment) succeed-mock fail-mock)
                     (check-mock-calls fail-mock (racket:list empty-arguments)))))
    (test-case "does not call succeed if first in sequence fails"
               (with-amb-mocks
                   (lambda (succeed-mock fail-mock)
                     (ambeval '(begin (amb) 1) (setup-environment) succeed-mock fail-mock)
                     (check-mock-calls succeed-mock racket:null))))
    (test-case "calls fail if second in sequence of three fails"
               (with-amb-mocks
                   (lambda (succeed-mock fail-mock)
                     (ambeval '(begin 1 (amb) 2) '() succeed-mock fail-mock)
                     (check-mock-calls fail-mock (racket:list empty-arguments)))))
    (test-case "does not call succeed if second in sequence of three fails"
               (with-amb-mocks
                   (lambda (succeed-mock fail-mock)
                     (ambeval '(begin 1 (amb) 2) (setup-environment) succeed-mock fail-mock)
                     (check-mock-calls succeed-mock racket:null)))))

   (test-suite
    "definition"
    (test-case "fails if definition is syntactically invalid"
               (with-amb-mocks
                   (lambda (succeed-mock fail-mock)
                     (check-exn racket:exn:fail? (lambda () (ambeval '(define x) (setup-environment) succeed-mock fail-mock))))))
    (test-case "evaluates to 'ok"
               (with-amb-mocks
                   (lambda (succeed-mock fail-mock)
                     (ambeval '(define x 2) (setup-environment) succeed-mock fail-mock)
                     (check-mock-result succeed-mock 'ok))))
    (test-case "adds binding to environment"
               (with-amb-mocks
                   (lambda (succeed-mock fail-mock)
                     (ambeval '(begin (define x 2) x) (setup-environment) succeed-mock fail-mock)
                     (check-mock-result succeed-mock 2))))
    (test-case "succeeds if value is defined twice"
               (with-amb-mocks
                   (lambda (succeed-mock fail-mock)
                     (ambeval '(begin (define x 2) (define x 3) x) (setup-environment) succeed-mock fail-mock)
                     (check-mock-result succeed-mock 3))))
    (test-case "creates functiond definition"
               (with-amb-mocks
                   (lambda (succeed-mock fail-mock)
                     (ambeval '(begin (define (f x) (+ x 1)) (f 1)) (setup-environment) succeed-mock fail-mock)
                     (check-mock-result succeed-mock 2)))))

   (test-suite
    "assignment"
    (test-case "fails if the assignment is syntactically invalid"
               (with-amb-mocks
                   (lambda (succeed-mock fail-mock)
                     (check-exn racket:exn:fail? (lambda () (ambeval '(set! x) (setup-environment) succeed-mock fail-mock))))))
    (test-case "doesn't assign if the value isn't already in the environment"
               (with-amb-mocks
                   (lambda (succeed-mock fail-mock)
                     (ambeval '(begin (set! x 3) x) (setup-environment) succeed-mock fail-mock)
                     (check-mock-result succeed-mock '(ERROR)))))
    (test-case "does not call fail if no evaluation failure occurs"
               (with-amb-mocks
                   (lambda (succeed-mock fail-mock)
                     (ambeval '(begin (define x 2) (set! x 3)) (setup-environment) succeed-mock fail-mock)
                     (check-mock-calls fail-mock racket:null))))
    (test-case "evaluates to 'ok"
               (with-amb-mocks
                   (lambda (succeed-mock fail-mock)
                     (ambeval '(begin (define x 2) (set! x 3)) (setup-environment) succeed-mock fail-mock)
                     (check-mock-result succeed-mock 'ok))))
    (test-case "set! adds the value to the environment"
               (with-amb-mocks
                   (lambda (succeed-mock fail-mock)
                     (ambeval '(begin (define x 2) (set! x 3) x) (setup-environment) succeed-mock fail-mock)
                     (check-mock-result succeed-mock 3)))))

   (test-suite
    "if"
    (test-case "fails if the if is syntactically invalid -- only if"
               (with-amb-mocks
                   (lambda (succeed-mock fail-mock)
                     (check-exn racket:exn:fail? (lambda () (ambeval '(if) (setup-environment) succeed-mock fail-mock))))))
    (test-case "fails if the if is syntactically invalid -- only condition"
               (with-amb-mocks
                   (lambda (succeed-mock fail-mock)
                     (check-exn racket:exn:fail? (lambda () (ambeval '(if true) (setup-environment) succeed-mock fail-mock))))))
    (test-case "evaluates to consequent if condition is true"
               (with-amb-mocks
                   (lambda (succeed-mock fail-mock)
                     (ambeval '(if true 1 2) (setup-environment) succeed-mock fail-mock)
                     (check-mock-result succeed-mock 1))))
    (test-case "evaluates to alternative if condition is false"
               (with-amb-mocks
                   (lambda (succeed-mock fail-mock)
                     (ambeval '(if false 1 2) (setup-environment) succeed-mock fail-mock)
                     (check-mock-result succeed-mock 2)))))

   (test-suite
    "application"
    (test-case "evaluates basic application"
               (with-amb-mocks
                   (lambda (succeed-mock fail-mock)
                     (ambeval '(+ 1 1) (setup-environment) succeed-mock fail-mock)
                     (check-mock-result succeed-mock 2)))))

   (test-suite
    "lambda"
    (test-case "evaluates basic procedure"
               (with-amb-mocks
                   (lambda (succeed-mock fail-mock)
                     (ambeval '((lambda (x) (+ x 1)) 1) (setup-environment) succeed-mock fail-mock)
                     (check-mock-result succeed-mock 2)))))

   (test-suite
    "let"
    (test-case "evaluates basic let"
               (with-amb-mocks
                   (lambda (succeed-mock fail-mock)
                     (ambeval '(let ((x 1) (y 1)) (+ x y)) (setup-environment) succeed-mock fail-mock)
                     (check-mock-result succeed-mock 2))))
    (test-case "evaluates basic let*"
               (with-amb-mocks
                   (lambda (succeed-mock fail-mock)
                     (ambeval '(let* ((x 1) (y (+ x 1))) y) (setup-environment) succeed-mock fail-mock)
                     (check-mock-result succeed-mock 2))))
    (test-case "evaluates named let"
               (with-amb-mocks
                   (lambda (succeed-mock fail-mock)
                     (let ((expr '(let dup ((i 0)
                                            (lst '(1 2 3)))
                                    (cond
                                      ((= i 1) (cons (car lst) lst))
                                      (else (cons (car lst) (dup (+ i 1) (cdr lst))))))))
                       (ambeval expr (setup-environment) succeed-mock fail-mock)
                       (check-mock-result succeed-mock (list 1 2 2 3)))))))

   (test-suite
    "cond"
    (test-case "evaluates to false when there are no branches"
               (with-amb-mocks
                   (lambda (succeed-mock fail-mock)
                     (ambeval '(cond) (setup-environment) succeed-mock fail-mock)
                     (check-mock-result succeed-mock #f)))))

   ))


(run-tests tests)