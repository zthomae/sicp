#lang sicp

(#%require (only racket/base error))

(define (lookup-variable-value exp env) (error "Not implemented -- LOOKUP-VARIABLE-VALUE"))
(define (make-procedure parameters body env) (error "Not implemented -- MAKE-PROCEDURE"))
(define (primitive-procedure? proc) #f)
(define (apply-primitive-procedure proc args) (error "Not implemented -- APPLY-PRIMITIVE-PROCEDURE"))
(define (compound-procedure? proc) #f)
(define (procedure-body proc) (error "Not implemented -- PROCEDURE-BODY"))
(define (extend-environment parameters args env) (error "Not implemented -- EXTEND-ENVIRONMENT"))
(define (procedure-parameters proc) (error "Not implemented -- PROCEDURE-PARAMETERS"))
(define (procedure-environment proc) (error "Not implemented -- PROCEDURE-ENVIRONMENT"))
(define (true? v) (error "Not implemented -- TRUE?"))
(define (set-variable-value! var val env) (error "Not implemented -- SET-VARIABLE-VALUE!"))
(define (define-variable! var val env) (error "Not implemented -- DEFINE-VARIABLE!"))

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
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
  (if (symbol? cadr exp)
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
(define (if-alternative e xp)
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