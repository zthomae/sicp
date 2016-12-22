#lang sicp

(#%require (only racket/base error)
           (rename r5rs apply-in-underlying-scheme apply))

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((let*? exp) (eval (let*->nested-lets exp) env))
        ((let? exp) (eval (let->combination exp) env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((while? exp) (eval (while->combination exp) env))
        ((until? exp) (eval (until->while exp) env))
        ((for? exp) (eval (for->while exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

;; begin exercise 4.1

(define (list-of-values-ltr exps env)
  (if (no-operands? exps)
      '()
      (let* ((first (eval (first-operand exps) env))
             (rest (list-of-values (rest-operands exps) env)))
        (cons first rest))))

(define (list-of-values-rtl exps env)
  (if (no-operands? exps)
      '()
      (let* ((rest (list-of-values (rest-operands exps) env))
             (first (eval (first-operand exps) env)))
        (cons first rest))))

;; end exercise 4.1

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (assignment? exp)
  (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)    ; formal parameters
                   (cddr exp))))  ; body

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond-alternate-form? clause)
  (eq? (car (cond-actions clause)) '=>))
(define (cond-alternate-form-proc clause) (cadr (cond-actions clause)))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (cond ((cond-else-clause? first)
               (if (null? rest)
                   (sequence->exp (cond-actions first))
                   (error "ELSE clause isn't last  -- COND->IF"
                          clauses)))
              ((cond-alternate-form? first)
               (list (make-lambda '(v f) (list (make-if 'v '(f v) (expand-clauses rest))))
                     (cond-predicate first)
                     (cond-alternate-form-proc first)))
              (else
               (make-if (cond-predicate first)
                        (sequence->exp (cond-actions first))
                        (expand-clauses rest)))))))

(define (eval-and exps env)
  (define (iter rest)
    (cond ((null? rest) true)
          (((car rest)) (iter (cdr rest)))
          (else false)))
  (iter (map (lambda (exp) (lambda () (eval exp env))) exps)))

(define (expand-and exps)
  (if (null? exps)
      true
      (make-if (car exps) (expand-and (cdr exps)) 'false)))

(define (eval-or exps env)
  (define (iter rest)
    (cond ((null? rest) false)
          (((car rest) true))
          (else (iter (cdr rest)))))
  (iter (map (lambda (exp) (lambda () (eval exp env))) exps)))

(define (expand-or exps)
  (if (null? exps)
      false
      (make-if (car exps) 'true (expand-or (cdr exps)))))

(define c1 '(cond ((assoc 'b '((a 1) (b 2))) => cadr) ((assoc 'a '((a 1) (b 2))) => cadr) (else (+ 1 3))))
(define c2 '(cond ((assoc 'b '((a 1) (b 2))) (+ 1 2)) ((assoc 'a '((a 1) (b 2))) (+ 1 2)) (else (+ 1 3))))

(define (expanded-c1)
  ((lambda (v f)
   (if v
       (f v)
       ((lambda (v f)
          (if v
              (f v)
              (+ 1 3)))
        (assoc 'a '((a 1) (b 2)))
        cadr)))
 (assoc 'b '((a 1) (b 2)))
 cadr))

(define (let? exp) (tagged-list? exp 'let))
(define (let-bindings exp) (cadr exp))
(define (let-body exp) (cddr exp))
(define (let-names exp) (map car (let-bindings exp)))
(define (let-values exp) (map cadr (let-bindings exp)))

(define (named-let? exp) (variable? (cadr exp)))
(define (named-let-name exp) (cadr exp))
(define (named-let-bindings exp) (caddr exp))
(define (named-let-parameters exp) (map car (named-let-bindings exp)))
(define (named-let-initial-values exp) (map cadr (named-let-bindings exp)))
(define (named-let-body exp) (cadddr exp))

(define (make-define binding val) (list 'define binding val))

(define (let->combination exp)
  (if (named-let? exp)
      (make-let
       '()
       (make-define
        (named-let-name exp)
        (make-lambda (named-let-parameters exp) (named-let-body exp)))
       (cons (named-let-name exp) (named-let-initial-values exp)))
      (let ((values (let-values exp))
            (proc (make-lambda (let-names exp) (let-body exp))))
        (if (null? let-values)
            (list proc)
            (cons proc values)))))

(define l '(let () 5 3))

(define l1
  '(let ((v1 e1)
         (v2 e2))
     (+ v1 v2)))

(define (make-let bindings . body)
  (append (list 'let bindings) body))

(define (let*? exp) (tagged-list? exp 'let*))

(define (let*->nested-lets exp)
  (define (iter bindings)
    (make-let
     (list (car bindings))
     (if (null? (cdr bindings))
         (let-body exp)
         (iter (cdr bindings)))))
  (if (null? (let-bindings exp))
      (let-body exp)
      (iter (let-bindings exp))))

(define l*
  '(let* ((x 3)
          (y (+ x 2))
          (z (+ x y 5)))
     (* x z)))

(define l*-result
  '(let ((x 3))
     (let ((y (+ x 2)))
       (let ((z (+ x y 5)))
         (* x z)))))

(define nl
  '(let fib-iter ((a 1)
                  (b 0)
                  (count n))
     (if (= count 0)
         b
         (fib-iter (+ a b) a (- count 1)))))

(define nl-expected
  '(let ()
     (define fib-iter
       (lambda (a b count)
         (if (= count 0)
             b
             (fib-iter (+ a b) a (- count 1)))))
     (fib-iter 1 0 n)))

;; while: while the predicate is true, continue to evaluate
;; the body expression. evaluates to the value of the last
;; execution of the body (or false if it never evaluated).
(define while-example
  '(while (> x 0)
          (begin
            (displayln x)
            (set! x (- x 1))
            x)))

(define while-transformed
  '(let ()
     (define while-iter
       (lambda (last-value)
         (if (> x 0)
             (while-iter
              (begin
                (displayln x)
                (set! x (- x 1))
                x))
             last-value)))
     (while-iter false)))

(define (while? exp) (tagged-list? exp 'while))
(define (while-predicate exp) (cadr exp))
(define (while-body exp) (caddr exp))

(define (while->combination exp)
  (make-let
   '()
   (make-define 'while-iter
                (make-lambda '(last-value)
                             (list (make-if (while-predicate exp)
                                            (list 'while-iter (while-body exp))
                                            'last-value))))
   '(while-iter false)))
                                               
;; until: the same as while, but with the predicate inverted
(define until-example
  '(until (= x 0)
          (begin
            (displayln x)
            (set! x (- x 1))
            x)))

(define (make-while predicate body) (list 'while predicate body))

(define (until? exp) (tagged-list? exp 'until))
(define (until-predicate exp) (cadr exp))
(define (until-body exp) (caddr exp))

(define (until->while exp)
  (make-while (list 'not (until-predicate exp)) (until-body exp)))

;; for: initialize a list of variables, and execute the body and the
;; continuing expression while the predicate evalutes to true. the
;; expression evaluates to the value of the continuing expression...
(define for-example
  '(for (((x 0) (y 0) (sum 0))
         (and (< x 5) (< x 5))
         (begin (set! x (+ x 1)) (set! y (+ y 1)) sum))
     (set! sum (+ sum x y))))

(define for-transformed
  '(let ((x 0) (y 0) (sum 0))
     (while (and (< x 5) (< x 5))
            (begin
              (set! sum (+ sum x y))
              (begin (set! x (+ x 1)) (set! y (+ y 1)) sum)))))

(define (for? exp) (tagged-list? exp 'for))
(define (for-control exp) (cadr exp))
(define (for-initialization exp) (car (for-control exp)))
(define (for-predicate exp) (cadr (for-control exp)))
(define (for-continue exp) (caddr (for-control exp)))
(define (for-body exp) (caddr exp))

(define (for->while exp)
  (make-let (for-initialization exp)
            (make-while (for-predicate exp)
                        (list 'begin (for-body exp) (for-continue exp)))))

(define nested-for
  (for->while
   `(for (((x 0) (sum 0))
          (< x 5)
          (begin (set! x (+ x 1)) sum))
      ,(for->while
        '(for (((y 0))
               (< y 5)
               (begin (set! y (+ y 1)) y))
           (set! sum (+ sum x y)))))))

(define nested-for-transformed
  '(let ((x 0) (sum 0))
     (while (< x 5)
            (begin
              (let ((y 0))
                (while (< y 5)
                       (begin
                         (set! sum (+ sum x y))
                         (begin (set! y (+ y 1)) y))))
              (begin (set! x (+ x 1)) sum)))))

(define inner-while
  '(while (< y 5)
          (begin
            (set! sum (+ sum x y))
            (begin (set! y (+ y 1)) y))))

(define nested-while
  `(let ((x 0) (sum 0))
     ,(while->combination
       `(while (< x 5)
               (begin
                 (let ((y 0))
                   ,(while->combination
                     '(while (< y 5)
                             (begin
                               (set! sum (+ sum x y))
                               (begin (set! y (+ y 1)) y)))))
                 (begin (set! x (+ x 1)) sum))))))

(define nested-while-transformed
  '(let ((x 0) (sum 0))
     (let ()
       (define while-iter
         (lambda (last-value)
           (if (< x 5)
               (while-iter
                (begin
                  (let ((y 0))
                    (let ()
                      (define while-iter
                        (lambda (last-value)
                          (if (< y 5)
                              (while-iter
                               (begin
                                 (set! sum (+ sum x y))
                                 (begin (set! y (+ y 1)) y)))
                              last-value)))
                      (while-iter false)))
                  (begin
                    (set! x (+ x 1))
                    sum)))
               last-value)))
       (while-iter false))))

(define (true? x) (not (eq? x false)))
(define (false? x) (eq? x false))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars) (env-loop (enclosing-environment env)))
            ((eq? var (car vars)) (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars) (env-loop (enclosing-environment env)))
            ((eq? var (car vals)) (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars) (add-binding-to-frame! var val frame))
            ((eq? var (car vars)) (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '< <)
        ;; more...
        ))

(define (primitive-procedure-names)
  (map car primitive-procedures))
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))
(define the-global-environment (setup-environment))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))
(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))
(define (announce-output string)
  (newline) (display string) (newline))
(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

;;; alternate environment functions -- exercise 4.11

(define the-empty-environment-2 '(head))

(define (lookup-variable-value-2 var env)
  (define (env-loop env)
    (define (scan bindings)
      (cond ((null? bindings) (env-loop (enclosing-environment env)))
            ((eq? var (caar bindings)) (cdar bindings))
            (else (scan (cdr bindings)))))
    (if (eq? env the-empty-environment-2)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (cdr frame)))))
  (env-loop env))

(define (extend-environment-2 bindings base-env)
  (cons (cons 'head bindings) base-env))

(define (add-binding-to-frame-2! var val frame)
  (if (null? (cdr frame))
      (set-cdr! frame (cons var val))
      (begin
        (set-cdr! (cdr frame) (cons (cadr frame) (cddr frame)))
        (set-car! (cdr frame) (cons var val)))))

(define (define-variable-2! var val env)
  (let ((frame (first-frame env)))
    (define (scan bindings)
      (cond ((null? bindings)
             (add-binding-to-frame-2! var val frame))
            ((eq? var (caar bindings))
             (set-car! (car bindings) val))
            (else (scan (cdr bindings)))))
    (scan (cdr frame))))

(define (set-variable-2! var val env)
  (define (env-loop env)
    (define (scan bindings)
      (cond ((null? bindings)
             (env-loop (enclosing-environment env)))
            ((eq? var (caar bindings))
             (set-cdr! (car bindings) val))
            (else (scan (cdr bindings)))))
    (if (eq? env the-empty-environment-2)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (cdr frame)))))
  (env-loop env))

(define (zip f lists)
  (cond ((null? lists) '())
        ((null? (car lists)) '())
        (else (cons (apply-in-underlying-scheme f (map car lists))
                    (zip f (map cdr lists))))))

(define (setup-environment-2)
  (let* ((bindings (zip cons (list (primitive-procedure-names) (primitive-procedure-objects))))
         (initial-env
          (extend-environment-2 bindings the-empty-environment-2)))
    (define-variable-2! 'true true initial-env)
    (define-variable-2! 'false false initial-env)
    initial-env))

(define the-global-environment-2 (setup-environment-2))

;;; abstracting variable lookup/setting

(define (search-env if-found if-not-found)
  (lambda (var env)
    (define (scan vars vals)
      (cond ((null? vars) (if-not-found env))
            ((eq? var (car vars)) (if-found vars vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame))))))

(define (lookup-variable-value-alt val env)
  ((search-env
    (lambda (vars vals) (car vals))
    (lambda (env) (lookup-variable-value-alt val (enclosing-environment env))))
   val env))

(define (set-variable-value-alt! var val env)
  ((search-env
    (lambda (vars vals) (set-car! vals val))
    (lambda (env) (set-variable-value-alt! var val (enclosing-environment env))))
   var env))

(define (define-variable!-alt var val env)
  ((search-env
    (lambda (vars vals) (set-car! vals val))
    (lambda (env) (add-binding-to-frame! var val (first-frame env))))
   var env))

;;; thoughts on 4.13

;; only attempting to unbind variables in the first frame is the
;; more predictable approach. if this is not so, multiple closures defined
;; from the same base environment could unbind each other's bindings.
;; and set! means that changing the value of these shared bindings _is_
;; an expected behavior that we can't wish away entirely.
;;
;; since we can redefine variables at arbitrary levels of the environment,
;; we don't need this feature to unbind primitives.
;;
;; I do not approve of this idea, but I am afraid to limit its power
;; because I can't think of how to use it. unbinding symbols in parent
;; environments seems to be completely fraught with danger to me, but
;; I don't know if I've thought enough to be justified in disallowing
;; it outright.

(define (make-unbound! var env)
  (define (scan vars vals)
    (cond ((null? (cdr vars))
           (error "Cannot unbind non-bound variable" var))
          ((eq? var (cadr vars))
           (begin
             (set-cdr! vars (cddr vars))
             (set-cdr! vals (cddr vals))))
          (else (scan (cdr vars) (cdr vals)))))
  (if (eq? env the-empty-environment)
      (error "Cannot unbind non-bound variable" var)
      (let ((frame (first-frame env)))
        (scan (cons 'vars (frame-variables frame))
              (cons 'vals (frame-values frame))))))