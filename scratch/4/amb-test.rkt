#lang sicp

(#%require "amb.rkt"
         rackunit
         rackunit/text-ui
         (only racket exn:fail?))

(define tests
  (test-suite
   "Amb interpreter tests"

   (test-suite
    "utils"

    (test-suite
     "true?"
     (test-case "(true? #t) is true"
                (check-true (true? #t)))
     (test-case "(true? #f) is false"
                (check-false (true? #f)))
     (test-case "(true? 0) is true"
                (check-true (true? 0))))

    (test-suite
     "false?"
     (test-case "(false? #f) is true"
                (check-true (false? #f)))
     (test-case "(false? #t) is false"
                (check-false (false? #t)))
     (test-case "(false? 0) is false"
                (check-false (false? 0))))

    (test-suite
     "any?"
     (test-case "(any? '()) is false"
                (check-false (any? '())))
     (test-case "(any? '(#t)) is true"
                (check-true (any? '(#t))))
     (test-case "(any? '(#t #f)) is true"
                (check-true (any? '(#t #f))))
     (test-case "(any? '(#f #f)) is false"
                (check-false (any? '(#f #f)))))

    (test-suite
     "partition"
     (test-case "(partition (lambda (x) (> x 0)) '()) is '(())"
                (check-equal? (partition (lambda (x) (> x 0)) '()) '(())))
     (test-case "(partition (lambda (x) (> x 0)) '(1)) is '((1))"
                (check-equal? (partition (lambda (x) (> x 0)) '(1)) '((1))))
     (test-case "(partition (lambda (x) (> x 0)) '(1 2 -1 5)) is '((1 2) -1 5)"
                (check-equal? (partition (lambda (x) (> x 0)) '(1 2 -1 5)) '((1 2) -1 5)))
     (test-case "(partition (lambda (x) (> x 0)) '(-1 5 2 0)) is '(() -1 5 2 0)"
                (check-equal? (partition (lambda (x) (> x 0)) '(-1 5 2 0)) '(() -1 5 2 0)))))

   (test-suite
    "expression detection"
    
    (test-suite
     "tagged-list?"
     (test-case "tagged-list? is true for list tagged with own car value"
                (let ((val '(a b c)))
                  (check-true (tagged-list? val (car val)))))
     (test-case "tagged-list? is false for a list tagged with something else"
                (check-false (tagged-list? '(a b c) 'b)))
     (test-case "tagged-list? is false for a non-list"
                (check-false (tagged-list? #f 'a))))
   
    (test-suite
     "error?"
     (test-case "(ERROR asdf) is an error"
                (check-true (error? '(ERROR asdf))))
     (test-case "error-value is an error"
                (check-true (error? error-value)))
     (test-case "empty list is not an error"
                (check-false (error? '())))
     (test-case "otherwise tagged list is not an error"
                (check-false (error? '(not-error))))
     (test-case "non-list is not an error"
                (check-false (error? 3))))

    (test-suite
     "assignment?"
     (test-case "(set! x 3) is an assignment"
                (check-true (assignment? '(set! x 3))))
     (test-case "(define x 3) is not an assignment"
                (check-false (assignment? '(define x 3))))
     (test-case "make-set creates an assignment"
                (check-true (assignment? (make-set 'x 3)))))

    (test-suite
     "definition?"
     (test-case "(define x 3) is a definition"
                (check-true (definition? '(define x 3))))
     (test-case "(let ((x 5)) x) is not a definition"
                (check-false (definition? '(let ((x 3)) x))))
     (test-case "make-define creates a definition"
                (check-true (definition? (make-define 'x 3)))))

    (test-suite
     "lambda?"
     (test-case "(lambda (x) x) is a lambda"
                (check-true (lambda? '(lambda (x) x))))
     (test-case "(define (f x) x) is not a lambda"
                (check-false (lambda? '(define (f x) x))))
     (test-case "make-lambda returns a lambda"
                (check-true (lambda? (make-lambda '(x) 'x)))))

    (test-suite
     "if?"
     (test-case "(if #t 0 1) is an if"
                (check-true (if? '(if #t 0 1))))
     (test-case "(unless #t 0 1) is not an if"
                (check-false (if? '(unless #t 0 1))))
     (test-case "make-if creates an if"
                (check-true (if? (make-if #f 1 0)))))

    (test-suite
     "begin?"
     (test-case "(begin x y) is a begin"
                (check-true (begin? '(begin x y))))
     (test-case "(begin x) is a begin"
                (check-true (begin? '(begin x))))
     (test-case "(let ((x 3)) 4 x) is not a begin"
                (check-false (begin? '(let ((x 3)) 4 x)))))

    (test-suite
     "cond?"
     (test-case "(cond ((#t 1) (#f 2))) is a cond"
                (check-true (cond? '(cond ((#t 1) (#f 2))))))
     (test-case "(if #t 0 1) is not a cond"
                (check-false (cond? '(if #t 0 1)))))

    (test-suite
     "quoted?"
     (test-case "''(x) is quoted"
                (check-true (quoted? ''(x))))
     (test-case "'(x) is not quoted"
                (check-false (quoted? '(x)))))

    (test-suite
     "amb?"
     (test-case "(amb a b) is an amb"
                (check-true (amb? '(amb a b))))
     (test-case "try-again is not an amb"
                (check-false (amb? 'try-again))))

    (test-suite
     "variable?"
     (test-case "x is a variable"
                (check-true (variable? 'x)))
     (test-case "3 is not a variable"
                (check-false (variable? '3))))

    (test-suite
     "self-evaluating?"
     (test-case "a number is self-evaluating"
                (check-true (self-evaluating? 3)))
     (test-case "a string is self-evaluating"
                (check-true (self-evaluating? "hello")))
     (test-case "a symbol is not self-evaluating"
                (check-false (self-evaluating? 'x))))

    (test-suite
     "application?"
     (test-case "(f x) is an application"
                (check-true (application? '(f x))))
     (test-case "((f x)) is an application"
                (check-true (application? '((f x)))))
     (test-case "((f x) y) is an application"
                (check-true (application? '((f x) y))))
     (test-case "\"asdf\" is not an application"
                (check-false (application? "asdf"))))

    (test-suite
     "let?"
     (test-case "(let ((y 1)) y) is a let"
                (check-true (let? '(let ((y 1)) y))))
     (test-case "(let x ((y 1)) y) is a let"
                (check-true (let? '(let x ((y 1)) y))))
     (test-case "(define x 4) is not a let"
                (check-false (let? '(define x 4))))
     (test-case "make-let creates a let"
                (check-true (let? (make-let '((x 1) (y (+ 1 2))) '(print asdf) '(+ x y))))))

    (test-suite
     "named-let?"
     (test-case "(let x ((y 1)) y) is a named let"
                (check-true (named-let? '(let x ((y 1)) y))))
     (test-case "(let ((y 1)) y) is not a named let"
                (check-false (named-let? '(let ((y 1)) y)))))

    (test-suite
     "let*?"
     (test-case "(let* ((x 1) (y 2)) x) is a let*"
                (check-true (let*? '(let* ((x 1) (y 2)) x))))
     (test-case "(let ((x 1) (y 2)) x) is not a let*"
                (check-false (let*? '(let ((x 1) (y 2)) x)))))

    (test-case "(letrec ((is-even? (lambda (n) (or (zero? n) (is-odd? (sub1 n))))) (is-odd? (lambda (n) (and (not (zero? n)) (is-even? (sub1 n))))))) (is-odd? 11) is a letrec"
               (check-true (letrec? '(letrec
                                         ((is-even? (lambda (n)
                                                      (or (zero? n)
                                                          (is-odd? (sub1 n)))))
                                          (is-odd? (lambda (n
                                                            (and (not (zero? n))
                                                                 (is-even? (sub1 n)))))))
                                       (is-odd? 11)))))
    
    (test-suite
     "primitive-procedure?"
     (test-case "(list 'primitive +) is a primitive procedure"
                (check-true (primitive-procedure? (list 'primitive +))))
     (test-case "'(procedure () 5 ()) is not a primitive procedure"
                (check-false (primitive-procedure? '(procedure () 5 ()))))
     (test-case "make-procedure does not create a primitive procedure"
                (check-false (primitive-procedure? (make-procedure '(x) '(+ x 1) '())))))

    (test-suite
     "compound-procedure?"
     (test-case "'(procedure () 5 ()) is a compound procedure"
                (check-true (compound-procedure? '(procedure () 5 ()))))
     (test-case "(list 'primitive +) is not a compound procedure"
                (check-false (compound-procedure? (list 'primitive +))))
     (test-case "make-procedure creates a compound procedure"
                (check-true (compound-procedure? (make-procedure '(x) '(+ x 1) '()))))))

   (test-suite
    "expression access"

    (test-case "text-of-quotation of ''asdf is asdf"
               (check-equal? (text-of-quotation ''asdf) 'asdf))

    (test-suite
     "assignments"
     (test-case "assignment-variable of (set! x 3) is x"
                (check-equal? (assignment-variable '(set! x 3)) 'x))
     (test-case "assignment-value of (set! x 3) is 3"
                (check-equal? (assignment-value '(set! x 3)) 3)))

    (test-suite
     "definitions"
     (test-case "definition-variable of (define x 3) is x"
                (check-equal? (definition-variable '(define x 3)) 'x))
     (test-case "definition-variable of (define (f x) 3) is f"
                (check-equal? (definition-variable '(define (f x) 3)) 'f))
     (test-case "definition-value of (define x 3) is 3"
                (check-equal? (definition-value '(define x 3)) 3))
     (test-case "definition-value of (define (f x) 3) is (lambda (x) 3)"
                (check-equal? (definition-value '(define (f x) 3))
                              '(lambda (x) 3))))

    (test-suite
     "lambda"
     (test-case "lambda-parameters of (lambda () 3) is '()"
                (check-equal? (lambda-parameters '(lambda () 3)) '()))
     (test-case "lambda-parameters of (lambda (x) 3) is '(x)"
                (check-equal? (lambda-parameters '(lambda (x) 3)) '(x)))
     (test-case "lambda-body of (lambda (x) 3) is '(3)"
                (check-equal? (lambda-body '(lambda (x) 3)) '(3)))
     (test-case "lambda-body of (lambda (x) (+ x 1)) is '((+ x 1))"
                (check-equal? (lambda-body '(lambda (x) (+ x 1))) '((+ x 1))))
     (test-case "lambda-body of (lambda () 0 1) is '(0 1)"
                (check-equal? (lambda-body '(lambda () 0 1)) '(0 1))))

    (test-suite
     "if"
     (test-case "if-predicate of (if #t 0 1) is #t"
                (check-equal? (if-predicate '(if #t 0 1)) #t))
     (test-case "if-predicate of (if (> 3 0) 0 1) is '(> 3 0)"
                (check-equal? (if-predicate '(if (> 3 0) 0 1)) '(> 3 0)))
     (test-case "if-predicate of (if (if (> 3 0) #t #f) 0 1) is '(if (> 3 0) #t #f)"
                (check-equal? (if-predicate '(if (if (> 3 0) #t #f) 0 1))
                              '(if (> 3 0) #t #f)))
     (test-case "if-consequent of (if #t 0 1) is 0"
                (check-equal? (if-consequent '(if #t 0 1)) 0))
     (test-case "if-consequent of (if #t (+ 1 2) 0) is '(+ 1 2)"
                (check-equal? (if-consequent '(if #t (+ 1 2) 0)) '(+ 1 2)))
     (test-case "if-alternative of (if #t 0 1) is 1"
                (check-equal? (if-alternative '(if #t 0 1)) 1))
     (test-case "if-alternative of (if #t 0 (+ 1 2)) is '(+ 1 2)"
                (check-equal? (if-alternative '(if #t 0 (+ 1 2))) '(+ 1 2)))
     (test-case "if-alternative of (if #t 0) is 'false"
                (check-equal? (if-alternative '(if #t 0)) 'false)))

    (test-suite
     "begin"
     (test-case "begin-actions of (begin 1 2) is '(1 2)"
                (check-equal? (begin-actions '(begin 1 2)) '(1 2)))
     (test-case "begin-actions of (begin (print asdf) (let ((x 1)) x)) is '((print asdf) (let ((x 1)) x))"
                (check-equal? (begin-actions '(begin (print asdf) (let ((x 1)) x)))
                              '((print asdf) (let ((x 1)) x))))
     (test-case "last-exp? of '() throws an error"
                (check-exn exn:fail? (lambda () (last-exp? '()))))
     (test-case "last-exp? of (1) is true"
                (check-true (last-exp? '(1))))
     (test-case "last-exp? of (1 2) is false"
                (check-false (last-exp? '(1 2))))
     (test-case "last-exp? of ((let ((x 1)) x)) is true"
                (check-true (last-exp? '((let ((x 1)) x)))))
     (test-case "last-exp? of ((print asdf) 3) is false"
                (check-false (last-exp? '((print asdf) 3))))
     (test-case "first-exp of '() throws an error"
                (check-exn exn:fail? (lambda () (first-exp '()))))
     (test-case "first-exp of (1) is 1"
                (check-equal? (first-exp '(1)) '1))
     (test-case "first-exp of (1 2) is 1"
                (check-equal? (first-exp '(1 2)) 1))
     (test-case "first-exp of ((print asdf) (let ((x 1)) x)) is '(print asdf)"
                (check-equal? (first-exp '((print asdf) (let ((x 1)) x))) '(print asdf)))
     (test-case "rest-exps of '() throws an error"
                (check-exn exn:fail? (lambda () (rest-exps '()))))
     (test-case "rest-exps of (1) is ()"
                (check-equal? (rest-exps '(1)) '()))
     (test-case "rest-exps of (1 2) is (2)"
                (check-equal? (rest-exps '(1 2)) '(2)))
     (test-case "rest-exps of ((print asdf) (let ((x 1)) x)) is '((let ((x 1)) x))"
                (check-equal? (rest-exps '((print asdf) (let ((x 1)) x))) '((let ((x 1)) x))))
     (test-case "rest-exps of (1 2 3) is '(2 3)"
                (check-equal? (rest-exps '(1 2 3)) '(2 3))))

    (test-suite
     "application"
     (test-case "operator of (+ 1 2) is '+"
                (check-equal? (operator '(+ 1 2)) '+))
     (test-case "operator of (f x) is 'f"
                (check-equal? (operator '(f x)) 'f))
     (test-case "operator of (exit) is 'exit"
                (check-equal? (operator '(exit)) 'exit))
     (test-case "operands of (+ 1 2) are '(1 2)"
                (check-equal? (operands '(+ 1 2)) '(1 2)))
     (test-case "operands of (f x) are '(x)"
                (check-equal? (operands '(f x)) '(x)))
     (test-case "operands of (exit) are '()"
                (check-equal? (operands '(exit)) '()))
     (test-case "no-operands? is false for '(x)"
                (check-false (no-operands? '(f x))))
     (test-case "no-operands? is false for '(1 2)"
                (check-false (no-operands? '(+ 1 2))))
     (test-case "no-operands? is true for '()"
                (check-true (no-operands? '())))
     (test-case "first-operand of (1 2) is 1"
                (check-equal? (first-operand '(1 2)) 1))
     (test-case "first-operand of (1) is 1"
                (check-equal? (first-operand '(1)) 1))
     (test-case "first-operand of '() throws an error"
                (check-exn exn:fail? (lambda () (first-operand '()))))
     (test-case "rest-operands of (1 2) is '(2)"
                (check-equal? (rest-operands '(1 2)) '(2)))
     (test-case "rest-operands of (1) is '()"
                (check-equal? (rest-operands '(1)) '()))
     (test-case "rest-operands of (1 2 3) is '(2 3)"
                (check-equal? (rest-operands '(1 2 3)) '(2 3)))
     (test-case "rest-operands throws on '()"
                (check-exn exn:fail? (lambda () (rest-operands '())))))

    (test-suite
     "cond"
     (test-case "cond-clauses of (cond (#t 1)) is '((#t 1))"
                (check-equal? (cond-clauses '(cond (#t 1))) '((#t 1))))
     (test-case "cond-clauses of (cond (#f 0) (#t 1)) is '((#f 0) (#t 1))"
                (check-equal? (cond-clauses '(cond (#f 0) (#t 1))) '((#f 0) (#t 1))))
     (test-case "cond-predicate of (#t 1) is #t"
                (check-equal? (cond-predicate '(#t 1)) #t))
     (test-case "cond-predicate of ((> 3 0) 1) is '(> 3 0)"
                (check-equal? (cond-predicate '((> 3 0) 1)) '(> 3 0)))
     (test-case "cond-else-clause? of (else 1) is true"
                (check-true (cond-else-clause? '(else 1))))
     (test-case "cond-else-clause? of (#t 1) is false"
                (check-false (cond-else-clause? '(#t 1))))
     (test-case "cond-else-clause? of ((> 3 0) 1) is false"
                (check-false (cond-else-clause? '((> 3 0) 1))))
     (test-case "cond-actions of (else 1) is '(1)"
                (check-equal? (cond-actions '(else 1)) '(1)))
     (test-case "cond-actions of (else 1 2) is '(1 2)"
                (check-equal? (cond-actions '(else 1 2)) '(1 2)))
     (test-case "cond-actions of ((> 3 0) (print asdf) 4) is '((print asdf) 4)"
                (check-equal? (cond-actions '((> 3 0) (print asdf) 4)) '((print asdf) 4)))
     (test-case "cond-actions of ((> 3 0)) is '()"
                (check-equal? (cond-actions '((> 3 0))) '()))
     (test-case "cond-alternate-form? of (else 1) is false"
                (check-false (cond-alternate-form? '(else 1))))
     (test-case "cond-alternate-form? of (else => (lambda (x) 1)) is true"
                (check-true (cond-alternate-form? '(else => (lambda (x) 1)))))
     (test-case "cond-alternate-form? of ((> 3 0) => (lambda (x) (+ x 1))) is true"
                (check-true (cond-alternate-form? '((> 3 0) => (lambda (x) (+ x 1))))))
     (test-case "cond-alternate-form-proc of (else => (lambda (x) 1)) is '(lambda (x) 1)"
                (check-equal? (cond-alternate-form-proc '(else => (lambda (x) 1))) '(lambda (x) 1)))
     (test-case "cond-alternate-form-proc of ((> 3 0) => (lambda (x) (+ 1 2))) is '(lambda (x) (+ 1 2))"
                (check-equal? (cond-alternate-form-proc '((> 3 0) => (lambda (x) (+ 1 2)))) '(lambda (x) (+ 1 2)))))

    (test-suite
     "let"
     (test-case "let-bindings of (let ((x 1)) x) is '((x 1))"
                (check-equal? (let-bindings '(let ((x 1)))) '((x 1))))
     (test-case "let-bindings of (let ((x 1) (y (+ 1 2)) x) is '((x 1) (y (+ 1 2))"
                (check-equal? (let-bindings '(let ((x 1) (y (+ 1 2))))) '((x 1) (y (+ 1 2)))))
     (test-case "let-bindings of (let () 1) is '()"
                (check-equal? (let-bindings '(let () 1)) '()))
     (test-case "let-body of (let ((x 1)) x) is '(x)"
                (check-equal? (let-body '(let ((x 1)) x)) '(x)))
     (test-case "let-body of (let ((x 1)) 1 x) is '(1 x)"
                (check-equal? (let-body '(let ((x 1)) 1 x)) '(1 x)))
     (test-case "let-body of (let () 1 2 3) is '(1 2 3)"
                (check-equal? (let-body '(let () 1 2 3)) '(1 2 3)))
     (test-case "let-body of (let ((x (+ 2 3) (y (> 3 4))) (+ x (if y 3 (+ 3 1)))) is '((+ x (if y 3 (+ 3 1)))))"
                (check-equal? (let-body '(let ((x (+ 2 3)) (y (> 3 4))) (+ x (if y 3 (+ 3 1)))))
                              '((+ x (if y 3 (+ 3 1))))))
     (test-case "let-names of (let ((x 1)) x) is '(x)"
                (check-equal? (let-names '(let ((x 1)) x)) '(x)))
     (test-case "let-names of (let ((x 1) (y (+ 2 1))) 4) is '(x y)"
                (check-equal? (let-names '(let ((x 1) (y (+ 2 1))) 4)) '(x y)))
     (test-case "let-names of (let () x) is '()"
                (check-equal? (let-names '(let () x)) '()))
     (test-case "let-values of (let ((x 1)) x) is (1)"
                (check-equal? (let-values '(let ((x 1)) x)) '(1)))
     (test-case "let-values of (let ((x 1) (y (+ 2 3))) 1) is '(1 (+ 2 3))"
                (check-equal? (let-values '(let ((x 1) (y (+ 2 3))))) '(1 (+ 2 3))))
     (test-case "let-values of (let () x) is '()"
                (check-equal? (let-values '(let () x)) '())))

    (test-suite
     "named-let"
     (test-case "named-let-name of (let f ((x 1)) 3) is 'f"
                (check-equal? (named-let-name '(let f ((x 1)) 3)) 'f))
     (test-case "named-let-bindings of (let f ((x 1)) 3) is '((x 1))"
                (check-equal? (named-let-bindings '(let f ((x 1)) 3)) '((x 1))))
     (test-case "named-let-bindings of (let f ((x 1) (y (+ 2 3))) 1) is '((x 1) (y (+ 2 3)))"
                (check-equal? (named-let-bindings '(let f ((x 1) (y (+ 2 3))) 1)) '((x 1) (y (+ 2 3)))))
     (test-case "named-let-bindings of (let f () 1) is '()"
                (check-equal? (named-let-bindings '(let f () 1)) '()))
     (test-case "named-let-parameters of (let f ((x 1)) 3) is '(x)"
                (check-equal? (named-let-parameters '(let f ((x 1)) 3)) '(x)))
     (test-case "named-let-parameters of (let f ((x 1) (y (+ 2 3))) 1) is '(x y)"
                (check-equal? (named-let-parameters '(let f ((x 1) (y (+ 2 3))) 1)) '(x y)))
     (test-case "named-let-parameters of (let f () 1) is '()"
                (check-equal? (named-let-parameters '(let f () 3)) '()))
     (test-case "named-let-initial-values of (let f ((x 1)) 3) is '(1)"
                (check-equal? (named-let-initial-values '(let f ((x 1)) 3)) '(1)))
     (test-case "named-let-initial-values of (let f ((x 1) (y (+ 2 3))) 1) is '(1 (+ 2 3))"
                (check-equal? (named-let-initial-values '(let f ((x 1) (y (+ 2 3))) 1)) '(1 (+ 2 3))))
     (test-case "named-let-initial-values of (let f () 1) is '()"
                (check-equal? (named-let-initial-values '(let f () 1)) '()))
     (test-case "named-let-body of (let f ((x 1)) (if (> x 10) 'done (begin (print asdf) (f (+ x 1)))) is '(if (> x 10) 'done (begin (print asdf) (f (+ x 1)))"
                (check-equal? (named-let-body '(let f ((x 1)) (if (> x 10) 'done (begin (print asdf) (f (+ x 1))))))
                              '(if (> x 10) 'done (begin (print asdf) (f (+ x 1))))))
     (test-case "named-let-body of (let f () 1 2) is 1"
                (check-equal? (named-let-body '(let f () 1 2)) 1)))

    (test-suite
     "procedures"
     (test-case "procedure-parameters of (procedure (x) (+ x 1) ()) is '(x)"
                (check-equal? (procedure-parameters '(procedure (x) (+ x 1) ())) '(x)))
     (test-case "procedure-parameters of (procedure (x y) (* x y) ()) is '(x y)"
                (check-equal? (procedure-parameters '(procedure (x y) (* x y) ())) '(x y)))
     (test-case "procedure-parameters of (procedure () (print asdf) ()) is '()"
                (check-equal? (procedure-parameters '(procedure () (print asdf) ())) '()))
     (test-case "procedure-body of (procedure (x) (+ x 1) ()) is '(+ x 1)"
                (check-equal? (procedure-body '(procedure (x) (+ x 1) ())) '(+ x 1)))
     (test-case "procedure-body of (procedure (x y) (* x y) ()) is '(* x y)"
                (check-equal? (procedure-body '(procedure (x y) (* x y) ())) '(* x y)))
     (test-case "procedure-body of (Procedure () (print asdf) ()) is '(print asdf)"
                (check-equal? (procedure-body '(procedure () (print asdf) ())) '(print asdf)))
     (test-case "procedure-environment of (procedure (x) (+ x 1) ()) is '()"
                (check-equal? (procedure-environment '(procedure (x) (+ x 1) ())) '()))
     (test-case "procedure-environment of (procedure (x y) (* x y) (((z) 1)) is '(((z) 1))"
                (check-equal? (procedure-environment '(procedure (x y) (* x y) (((z) 1)))) '(((z) 1))))
     (test-case "primitive-implementation of (list 'primitive +) is +"
                (check-equal? (primitive-implementation (list 'primitive +)) +)))

    (test-suite
     "amb"
     (test-case "amb-choices of (amb 1) is '(1)"
                (check-equal? (amb-choices '(amb 1)) '(1)))
     (test-case "amb-choices of (amb 1 2 3) is '(1 2 3)"
                (check-equal? (amb-choices '(amb 1 2 3)) '(1 2 3)))
     (test-case "amb-choices of (amb) is '()"
                (check-equal? (amb-choices '(amb)) '()))))
   
   (test-suite
    "environment handling"

    (test-suite
     "extend-environment"
     (test-case "fails if more variables than values"
                (check-true (error? (extend-environment '(x) '() the-empty-environment))))
     (test-case "fails if more values than variables"
                (check-true (error? (extend-environment '() '(2) the-empty-environment))))
     (test-case "adds a single variable and value to the environment"
                (check-equal? (extend-environment '(x) '(1) the-empty-environment)
                              '(((x) 1))))
     (test-case "adds two bindings to the environment"
                (check-equal? (extend-environment '(x y) '(1 2) the-empty-environment)
                              '(((x y) 1 2))))
     (test-case "nests new environments"
                (check-equal? (extend-environment '(y) '(2)
                                                  (extend-environment '(x) '(1) the-empty-environment))
                              '(((y) 2) ((x) 1)))))
    
    (test-suite
     "enclosing-environment"
     (test-case "the empty environment has no enclosing environment"
                (check-exn exn:fail? (lambda () (enclosing-environment the-empty-environment))))
     (test-case "enclosing environment is empty"
                (check-equal? (enclosing-environment '(((x) 1)))
                              the-empty-environment))
     (test-case "enclosing environment is not empty"
                (check-equal? (enclosing-environment
                               '(((y) "asdf") ((x y) 1 2)))
                              '(((x y) 1 2))))))

   (test-suite
    "syntactic transformations"

    (test-suite
     "sequence->exp"
     (test-case "(sequence->exp nil) is nil"
                (check-equal? (sequence->exp nil) nil))
     (test-case "sequence->exp of a list of one expression is that expression"
                (check-equal? (sequence->exp '(e)) 'e))
     (test-case "(sequence->exp e1 e2) is (begin e1 e2)"
                (check-equal? (sequence->exp '(e1 e2)) '(begin e1 e2)))
     (test-case "(sequence->exp e1 e2 e3) is (begin e1 e2 e3)"
                (check-equal? (sequence->exp '(e1 e2 e3)) '(begin e1 e2 e3))))

    (test-suite
     "expand-clauses"
     (test-case "(expand-clauses nil) is 'false"
                (check-equal? (expand-clauses nil) 'false))
     (test-case "(expand-clauses '((p1 e1) (p2 e2)) is '(if p1 e1 (if p2 e2 'false))"
                (check-equal? (expand-clauses '((p1 e1) (p2 e2)))
                              '(if p1 e1 (if p2 e2 false))))
     (test-case "(expand-clauses '((p1 e1) (p2 e2) (else e3)) is '(if p1 e1 (if p2 e2 e3))"
                (check-equal? (expand-clauses '((p1 e1) (p2 e2) (else e3)))
                              '(if p1 e1 (if p2 e2 e3))))
     (test-case "returns an error if else clauses is not first"
                (check-true (error? (expand-clauses '((p1 e1) (else e2) (p3 e3))))))
     (test-case "(expand-clauses '((p1 => e1) (p2 => e2)) returns '(lambda (v f) (if (v (f v) ((lambda (v f) (if v (f v) false)) p2 e2))) p1 e1)"
                (check-equal? (expand-clauses '((p1 => e1) (p2 => e2)))
                              '((lambda (v f)
                                  (if v
                                      (f v)
                                      ((lambda (v f)
                                         (if v
                                             (f v)
                                             false))
                                       p2
                                       e2)))
                                p1
                                e1)))
     (test-case "(expand-clauses '((p1 e1) (p2 => e2) (else e3)) is '(if p1 e1 ((lambda (v f) (if v (f v) e3)) p2 e2)"
                (check-equal? (expand-clauses '((p1 e1) (p2 => e2) (else e3)))
                              '(if p1 e1 ((lambda (v f) (if v (f v) e3)) p2 e2))))
     (test-case "expand-clauses doesn't accept else in alternate form"
                (check-true (error? (expand-clauses '((p1 e1) (else => e2))))))
     (test-case "expand-clauses propagates errors through nesting"
                (check-true (error? (expand-clauses '((p1 e1) (p2 e2) (else e3) (p4 e4))))))
     (test-case "expand-clauses propagates errors in alternate form"
                (check-true (error? (expand-clauses '((p1 => e1) (p2 e2) (else e3) (p4 e4)))))))

    (test-suite
     "cond->if"
     (test-case "(cond->if '(cond (p1 e1) (p2 e2) (else e3)) is '(if p1 e1 (if p2 e2 e3))"
                (check-equal? (cond->if '(cond (p1 e1) (p2 e2) (else e3)))
                              '(if p1 e1 (if p2 e2 e3))))
     (test-case "(cond->if '(cond (p1 e1) (else e2) (p3 e3)) is an error"
                (check-true (error? (cond->if '(cond (p1 e1) (else e2) (p3 e3))))))
     (test-case "(cond->if '(cond (p1 => e1) (p2 => e2) (p3 e3) (else e4))) is ((lambda (v f) (if v (f v) ((lambda (v f) (if v (f v) (if p3 e3 e4))) p2 e2))) p1 e1)"
                (check-equal? (cond->if '(cond (p1 => e1) (p2 => e2) (p3 e3) (else e4)))
                              '((lambda (v f)
                                  (if v
                                      (f v)
                                      ((lambda (v f)
                                         (if v
                                             (f v)
                                             (if p3 e3 e4)))
                                       p2
                                       e2)))
                                p1
                                e1)))
     (test-case "(cond->if '(cond)) is false"
                (check-equal? (cond->if '(cond)) 'false)))

    (test-suite
     "let->combination"
     (test-case "(let->combination '(let ((v1 e1)) e2)) is '((lambda (v1) e2) e1)"
                (check-equal? (let->combination '(let ((v1 e1)) e2))
                              '((lambda (v1) e2) e1)))
     (test-case "(let->combination '(let ((v1 e1) (v2 e2)) e3)) is '((lambda (v1 v2) e3) e1 e2)"
                (check-equal? (let->combination '(let ((v1 e1) (v2 e2)) e3))
                              '((lambda (v1 v2) e3) e1 e2)))
     (test-case "(let->combination '(let () e1) is '((lambda () e1))"
                (check-equal? (let->combination '(let () e1))
                              '((lambda () e1))))
     (test-case "(let->combination '(let f ((v1 e1) (v2 e2)) e3)) is '(let () (define f (lambda (v1 v2) . e3)) (f e1 e2))"
                (check-equal? (let->combination '(let f ((v1 e1) (v2 e2)) e3))
                              '(let () (define f (lambda (v1 v2) . e3)) (f e1 e2)))))

    (test-suite
     "let*->nested-lets"
     (test-case "(let*->nested-lets of '(let* ((v1 e1) (v2 e2)) e3)) is '(let ((v1 e1)) (let ((v2 e2)) e3))"
                (check-equal? (let*->nested-lets '(let* ((v1 e1) (v2 e2)) e3))
                              '(let ((v1 e1)) (let ((v2 e2)) e3))))))))


(run-tests tests)