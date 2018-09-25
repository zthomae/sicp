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

(define (check-last-n-mock-results mock n result)
  (define (drop n lst)
    (cond ((= n 0) lst)
          ((racket:null? lst) nil)
          (else (drop (- n 1) (racket:cdr lst)))))
  (define (n-last n lst)
    (drop (- (racket:length lst) n) lst))

  (check-equal? (racket:map racket:car
                            (n-last n
                                    (racket:map
                                     (lambda (call) (arguments-positional (mock-call-args call)))
                                     (mock-calls mock))))
                result))
    
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

   (test-suite
    "permanent-set!"
    (test-case "book example"
               (with-amb-mocks
                   (lambda (succeed-mock fail-mock)
                     (let ((exprs '((define count 0)
                                    (define (an-element-of items)
                                      (require (not (null? items)))
                                      (amb (car items) (an-element-of (cdr items))))
                                    (let ((x (an-element-of (quote (a b c))))
                                          (y (an-element-of (quote (a b c)))))
                                      (permanent-set! count (+ count 1))
                                      (require (not (eq? x y)))
                                      (list x y count))))
                           (env (setup-environment)))
                       (for-each (lambda (expr) (ambeval expr env succeed-mock fail-mock)) exprs)
                       (check-last-n-mock-results succeed-mock 1 (racket:list '(a b 2))))))))

   (test-suite
    "if-fail"
    (test-case "returns failure value if no cases pass"
               (with-amb-mocks
                   (lambda (succeed-mock fail-mock)
                     (ambeval '(if-fail (let ((x (amb 1 3 5)))
                                          (require (> x 5))
                                          x)
                                        'all-small)
                              (setup-environment)
                              succeed-mock
                              fail-mock)
                     (check-mock-result succeed-mock 'all-small))))
    (test-case "returns first successful value"
               (with-amb-mocks
                   (lambda (succeed-mock fail-mock)
                     (ambeval '(if-fail (let ((x (amb 1 3 5 8)))
                                          (require (> x 5))
                                          x)
                                        'all-small)
                              (setup-environment)
                              succeed-mock
                              fail-mock)
                     (check-mock-result succeed-mock 8)))))

   ;; TODO: Add tests for ramb when it is possible to control backtracking outside of the driver loop
   ;; (i.e. when try-again is extracted as a special form)

   (test-suite
    "require"
    (test-case "should accept basic true predicate"
               (with-amb-mocks
                   (lambda (succeed-mock fail-mock)
                     (ambeval '(require (> 1 0)) (setup-environment) succeed-mock fail-mock)
                     (check-mock-result succeed-mock 'ok))))
    (test-case "should fail on false predicate"
               (with-amb-mocks
                   (lambda (succeed-mock fail-mock)
                     (ambeval '(require (> 0 1)) (setup-environment) succeed-mock fail-mock)
                     (check-mock-calls fail-mock (racket:list empty-arguments)))))
    (test-case "should accept true predicate with amb"
               (with-amb-mocks
                   (lambda (succeed-mock fail-mock)
                     (ambeval '(require (> 1 (amb 0 1 2 3))) (setup-environment) succeed-mock fail-mock)
                     (check-mock-result succeed-mock 'ok)))))
   
   (test-suite
    "complex examples"
    (test-case "pythagorean triples"
               (with-amb-mocks
                   (lambda (succeed-mock fail-mock)
                     (let ((exprs '((define (an-integer-starting-from n)
                                      (amb n (an-integer-starting-from (+ n 1))))

                                    (define (require p)
                                      (if (not p) (amb)))

                                    (define (an-integer-between i j)
                                      (require (< i j))
                                      (amb i (an-integer-between (+ i 1) j)))

                                    (define (pythagorean-triples-sum)
                                      (let ((sum (an-integer-starting-from 3)))
                                        (let ((i (an-integer-between 1 sum)))
                                          (let ((j (an-integer-between i sum)))
                                            (let ((k (an-integer-between j sum)))
                                              (require (= sum (+ i j k)))
                                              (require (= (+ (* i i) (* j j)) (* k k)))
                                              (list i j k))))))

                                    (define (pythagorean-triples)
                                      (let ((k (an-integer-starting-from 1)))
                                        (let ((i (an-integer-between 1 k)))
                                          (let ((j (an-integer-between i k)))
                                            (require (= (+ (* i i) (* j j)) (* k k)))
                                            (list i j k)))))

                                    (define (a-pythagorean-triple-between low high)
                                      (let ((i (an-integer-between low high))
                                            (hsq (* high high)))
                                        (let ((j (an-integer-between i high)))
                                          (let ((ksq (+ (* i i) (* j j))))
                                            (require (>= hsq ksq))
                                            (let ((k (sqrt ksq)))
                                              (require (integer? k))
                                              (list i j k))))))

                                    (a-pythagorean-triple-between 1 10)
                                    (a-pythagorean-triple-between 10 20)
                                    ))
                           (env (setup-environment)))
                       (for-each (lambda (expr) (ambeval expr env succeed-mock fail-mock)) exprs)
                       (check-last-n-mock-results succeed-mock 2 (racket:list '(3 4 5) '(12 16 20)))))))

    (test-case "liars problem"
               (with-amb-mocks
                   (lambda (succeed-mock fail-mock)
                     (let ((exprs '((define (require p)
                                      (if (not p) (amb)))

                                    (define (xor a b)
                                      (cond (a (not b))
                                            (else b)))

                                    (define (distinct? items)
                                      (cond ((null? items) true)
                                            ((null? (cdr items)) true)
                                            ((member (car items) (cdr items)) false)
                                            (else (distinct? (cdr items)))))

                                    (define (liars)
                                      (let ((betty (amb 1 2 3 4 5))
                                            (ethel (amb 1 2 3 4 5))
                                            (joan (amb 1 2 3 4 5))
                                            (kitty (amb 1 2 3 4 5))
                                            (mary (amb 1 2 3 4 5)))
                                        (require (xor (= kitty 2) (= betty 3)))
                                        (require (xor (= ethel 1) (= joan 2)))
                                        (require (xor (= joan 3) (= ethel 5)))
                                        (require (xor (= kitty 2) (= mary 4)))
                                        (require (xor (= mary 4) (= betty 1)))
                                        (require (distinct? (list betty ethel joan kitty mary)))
                                        (list (list 'betty betty)
                                              (list 'ethel ethel)
                                              (list 'joan joan)
                                              (list 'kitty kitty)
                                              (list 'mary mary))))

                                    (liars)))
                           (env (setup-environment)))
                       (for-each (lambda (expr) (ambeval expr env succeed-mock fail-mock)) exprs)
                       (check-last-n-mock-results succeed-mock 1 (racket:list (list (list 'betty 3)
                                                                                    (list 'ethel 5)
                                                                                    (list 'joan 2)
                                                                                    (list 'kitty 1)
                                                                                    (list 'mary 4)))))))))
   ))


(run-tests tests)