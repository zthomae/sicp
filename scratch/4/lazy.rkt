#lang sicp

(#%require (rename r5rs apply-in-underlying-scheme apply)
           (only racket print-as-expression))

(print-as-expression #f)

(define (error . args)
  (newline)
  (display "ERROR: ")
  (for-each (lambda (a) (display a) (display " ")) args)
  (newline)
  error-value)

(define (error? exp)
  (tagged-list? exp 'ERROR))

(define error-value (list 'ERROR))

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((error? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp)
         (let ((rest (cadr exp)))
           (if (list? rest)
               (eval (transformed-quotation rest) env)
               rest)))
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
        ((application? exp)
         (apply (actual-value (operator exp) env)
                (operands exp)
                env))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (actual-value exp env)
  (force-it (eval exp env)))

(define (force-it obj)
  (cond ((memo-thunk? obj)
         (let ((result (actual-value
                        (thunk-exp obj)
                        (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr obj) result)
           (set-cdr! (cdr obj) '())
           result))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        ((thunk? obj)
         (actual-value (thunk-exp obj) (thunk-env obj)))
        (else obj)))

;; (define (force-it obj)
;;   (if (thunk? obj)
;;       (actual-value (thunk-exp obj) (thunk-env obj))
;;       obj))

(define (delay-it exp env)
  (list 'thunk exp env))

(define (thunk? obj)
  (tagged-list? obj 'thunk))

(define (delay-it-memo exp env)
  (list 'thunk-memo exp env))

(define (memo-thunk? obj)
  (tagged-list? obj 'thunk-memo))

(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))

(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))

(define (any? xs)
  (if (null? xs)
      #f
      (or (car xs) (any? (cdr xs)))))

(define (apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (let ((args (list-of-arg-values arguments env)))
           (if (any? (map error? args))
               error-value
               (apply-primitive-procedure
                procedure
                args))))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (map parameter-name (procedure-parameters procedure))
           (list-of-mixed-arg-values (zip-with-types (procedure-parameters procedure) arguments) env)
           (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

(define (is-strict-param? p) (symbol? p))
(define (is-lazy-param? p) (and (pair? p) (eq? (parameter-type p) 'lazy)))
(define (is-lazy-memo-param? p) (and (pair? p) (eq? (parameter-type p) 'lazy-memo)))

(define (parameter-name p) (if (pair? p) (car p) p))
(define (parameter-type p) (if (pair? p) (cadr p) 'strict))

(define (zip-with-types parameters arguments)
  (define (loop params args)
    (if (null? params)
        '()
        (cons (list (car args) (parameter-type (car params)))
              (loop (cdr params) (cdr args)))))
  (loop parameters arguments))

(define (argument-exp exp) (car exp))
(define (argument-type exp) (cadr exp))

(define (list-of-mixed-arg-values exps env)
  (if (no-operands? exps)
      '()
      (let* ((first (first-operand exps))
             (exp (argument-exp first))
             (type (argument-type first))
             (next-value
              (cond ((eq? type 'strict) (actual-value exp env))
                    ((eq? type 'lazy) (delay-it exp env))
                    ((eq? type 'lazy-memo) (delay-it-memo exp env))
                    (else (error "Unknown laziness type -- LIST-OF-MIXED-ARG-VALUES" first)))))
        (cons next-value (list-of-mixed-arg-values (rest-operands exps)
                                                   env)))))

(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps) env)
            (list-of-arg-values (rest-operands exps)
                                env))))

(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
      '()
      (cons (delay-it (first-operand exps) env)
            (list-of-delayed-args (rest-operands exps)
                                  env))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
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

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (transformed-quotation exp)
  (accumulate (lambda (next acc)
                (list 'cons
                      (if (list? next) (transformed-quotation next) (list 'quote next))
                      acc))
              nil
              exp))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

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

(define (true? x) (not (eq? x false)))
(define (false? x) (eq? x false))

(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-defines body) env))
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
            ((eq? var (car vars))
             (let ((val (car vals)))
               (if (eq? val '*unassigned*)
                   (error "Use of unassigned variable" var)
                   val)))
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
            ((eq? var (car vars)) (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (if (error? val)
      error-value
      (env-loop env)))

(define (define-variable! var val env)
  (if (error? val)
      error-value
      (let ((frame (first-frame env)))
        (define (scan vars vals)
          (cond ((null? vars) (add-binding-to-frame! var val frame))
                ((eq? var (car vars)) (set-car! vals val))
                (else (scan (cdr vars) (cdr vals)))))
        (scan (frame-variables frame)
              (frame-values frame)))))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'null? null?)
        (list '+ +)
        (list '- -)
        (list '< <)
        (list '> >)
        (list '= =)
        (list 'true #t)
        (list 'false #f)
        (list '* *)
        (list '/ /)
        ;; more...
        ))

(define (primitive-procedure-names)
  (map car primitive-procedures))
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (setup-non-primitives initial-env)
  (eval '(define (cons (x lazy) (y lazy))
             (lambda ((m lazy)) (m x y)))
          initial-env)
  (eval '(define (car (z lazy))
           (z (lambda ((p lazy) (q lazy)) p)))
        initial-env)
  (eval '(define (cdr (z lazy))
           (z (lambda ((p lazy) (q lazy)) q)))
        initial-env)
  initial-env)

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

(define input-prompt ";;; L-Eval input:")
(define output-prompt ";;; L-Eval value:")

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

(define (partition pred lst)
  (define (go acc rest)
    (if (null? rest)
        (cons (reverse acc) '())
        (let ((x (car rest))
              (xs (cdr rest)))
          (if (pred x)
              (go (cons x acc) xs)
              (cons (reverse acc) rest)))))
  (go '() lst))

(define (make-set var val)
  (list 'set! var val))

(define (scan-out-defines procedure-body)
  (let* ((define-partition (partition definition? procedure-body))
         (defines (car define-partition))
         (body (cdr define-partition)))
    (if (null? defines)
        body
        (let ((unassigned-bindings
               (map (lambda (d) (list (definition-variable d) ''*unassigned*)) defines))
              (set-expressions
               (map (lambda (d) (make-set (definition-variable d) (definition-value d))) defines)))
          (list (append (list 'let unassigned-bindings) (append set-expressions body)))))))

(define (letrec->nested-let exp)
  (let ((names (let-names exp))
        (body (let-body exp)))
    (let ((unassigned-bindings
           (map (lambda (n) (list n '*unassigned)) names))
          (set-expressions
           (map (lambda (p) (make-set (car p) (cdr p))) (let-bindings exp))))
      (append (list 'let unassigned-bindings) (append set-expressions body)))))

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (setup-non-primitives initial-env)))

(define the-global-environment (setup-environment))

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (actual-value input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))
