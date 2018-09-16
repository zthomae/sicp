#lang sicp

(#%require (rename r5rs apply-in-underlying-scheme apply)
           (prefix racket: racket))

(racket:provide setup-environment ambeval driver-loop)

(racket:print-as-expression #f)

(define (error . args)
  (racket:error args))

(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze (let->combination exp)))
        ((let*? exp) (analyze (let->combination (let*->nested-lets exp))))
        ((amb? exp) (analyze-amb exp))
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))

(define (any? xs)
  (if (null? xs)
      #f
      (or (car xs) (any? (cdr xs)))))

;; copypasta from scheme.rkt

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
               (cond ((cond-alternate-form? first)
                      (error "ELSE clause isn't allowed in alternate form" clauses))
                     ((null? rest) (sequence->exp (cond-actions first)))
                     (else (error "ELSE clause isn't last -- COND->IF" clauses))))
              ((cond-alternate-form? first)
               (let ((rest-expanded (expand-clauses rest)))
                 (list (make-lambda '(v f) (list (make-if 'v '(f v) rest-expanded)))
                       (cond-predicate first)
                       (cond-alternate-form-proc first))))
              (else
               (let ((rest-expanded (expand-clauses rest)))
                 (make-if (cond-predicate first)
                          (sequence->exp (cond-actions first))
                          rest-expanded)))))))

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
(define (named-let-body exp) (cdddr exp))

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
    (if (null? (cdr bindings))
        (apply make-let (cons bindings (let-body exp)))
        (make-let (list (car bindings)) (iter (cdr bindings)))))
  (if (null? (let-bindings exp))
      (let-body exp)
      (iter (let-bindings exp))))

(define (true? x) (not (eq? x false)))
(define (false? x) (eq? x false))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env)) ;; TODO: scan-out-defines
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
        (list '- -)
        (list '* *)
        (list '< <)
        (list '> >)
        (list '>= >=)
        (list 'not not)
        (list '= =)
        (list 'abs abs)
        (list 'list list)
        (list 'integer? integer?)
        (list 'sqrt sqrt)
        (list 'member member)
        (list 'eq? eq?)
        (list 'memq memq)
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

;; amb
(define (amb? exp) (tagged-list? exp 'amb))
(define (amb-choices exp) (cdr exp))

(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))

;; (define (analyze-self-evaluating exp)
;;   (lambda (env) exp))
(define (analyze-self-evaluating exp)
  (lambda (env succeed fail)
    (succeed exp fail)))

;; (define (analyze-quoted exp)
;;   (let ((qval (text-of-quotation exp)))
;;     (lambda (env) qval)))
(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail)
      (succeed qval fail))))

;; (define (analyze-variable exp)
;;   (lambda (env) (lookup-variable-value exp env)))
(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env)
             fail)))

;; (define (analyze-lambda exp)
;;   (let ((vars (lambda-parameters exp))
;;         (bproc (analyze-sequence (lambda-body exp))))
;;     (lambda (env) (make-procedure vars bproc env))))
(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env succeed fail)
      (succeed (make-procedure vars bproc env) fail))))

;; (define (analyze-if exp)
;;   (let ((pproc (analyze (if-predicate exp)))
;;         (cproc (analyze (if-consequent exp)))
;;         (aproc (analyze (if-alternative exp))))
;;     (lambda (env)
;;       (if (true? (pproc env))
;;           (cproc env)
;;           (aproc env)))))
(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (pproc env
             ;; success continuation for evaluating the predicate
             ;; to obtain pred-value
             (lambda (pred-value fail2)
               (if (true? pred-value)
                   (cproc env succeed fail2)
                   (aproc env succeed fail2)))
             ;; failure continuation for evaluating the predicate
             fail))))

;; (define (analyze-sequence exps)
;;   (define (sequentially proc1 proc2)
;;     (lambda (env) (proc1 env) (proc2 env)))
;;   (define (loop first-proc rest-procs)
;;     (if (null? rest-procs)
;;         first-proc
;;         (loop (sequentially first-proc (car rest-procs))
;;               (cdr rest-procs))))
;;   (let ((procs (map analyze exps)))
;;     (if (null? procs)
;;         (error "Empty sequence -- ANALYZE"))
;;     (loop (car procs) (cdr procs))))
(define (analyze-sequence exps)
  (define (sequentially a b)
    (lambda (env succeed fail)
      (a env
         ;; success continuation for calling a
         (lambda (a-value fail2)
           (b env succeed fail2))
         ;; failure continuation for calling a
         fail)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))

;; (define (analyze-definition exp)
;;   (let ((var (definition-variable exp))
;;         (vproc (analyze (definition-value exp))))
;;     (lambda (env)
;;       (define-variable! var (vproc env) env)
;;       'ok)))
(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env succeed fail)
      (vproc env
             ;; success continuation for if the value
             ;; to be assigned to var successfully
             ;; evaluates
             (lambda (val fail2)
               (define-variable! var val env)
               (succeed 'ok fail2))
             ;; failure continuation if it does not
             fail))))

;; (define (analyze-assignment exp)
;;   (let ((var (assignment-variable exp))
;;         (vproc (analyze (assignment-value exp))))
;;     (lambda (env)
;;       (set-variable-value! var (vproc env) env)
;;       'ok)))
(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (let ((old-value (lookup-variable-value var env)))
                 (set-variable-value! var val env)
                 (succeed 'ok
                          (lambda ()
                            (set-variable-value! var old-value env)
                            (fail2)))))
             fail))))

;; (define (analyze-application exp)
;;   (let ((fproc (analyze (operator exp)))
;;         (aprocs (map analyze (operands exp))))
;;     (lambda (env)
;;       (execute-application (fproc env)
;;                            (map (lambda (aproc) (aproc env))
;;                                 aprocs)))))
(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env succeed fail)
      (fproc env
             (lambda (proc fail2)
               (get-args aprocs
                         env
                         (lambda (args fail3)
                           (execute-application
                            proc args succeed fail3))
                         fail2))
             fail))))

(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
      (succeed '() fail)
      ((car aprocs) env
                    ;; success continuation for this aproc
                    (lambda (arg fail2)
                      (get-args (cdr aprocs)
                                env
                                ;; success continuation for recursive
                                ;; call to get-args
                                (lambda (args fail3)
                                  (succeed (cons arg args)
                                           fail3))
                                fail2))
                    fail)))

(define (execute-application proc args succeed fail)
  (cond ((primitive-procedure? proc)
         (succeed (apply-primitive-procedure proc args)
                  fail))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))
          succeed
          fail))
        (else
         (error
          "Unknown procedure type -- EXECUTE-APPLICATION"
          proc))))

(define (analyze-amb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            ((car choices) env
                           succeed
                           (lambda () (try-next (cdr choices))))))
      (try-next cprocs))))

;; (define input-prompt ";;; M-Eval input:")
;; (define output-prompt ";;; M-Eval value:")

;; (define (driver-loop)
;;   (prompt-for-input input-prompt)
;;   (let ((input (read)))
;;     (let ((output (eval input the-global-environment)))
;;       (announce-output output-prompt)
;;       (user-print output)))
;;   (driver-loop))

(define input-prompt ";;; Amb-Eval input:")
(define output-prompt ";;; Amb-Eval value:")

(define (driver-loop)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ((input (read)))
      (cond ((eq? input 'try-again) (try-again))
            ((racket:eof-object? input) 'done)
            (else
             (begin
               (newline)
               (display ";;; Starting a new problem ")
               (racket:with-handlers
                ((racket:exn:fail? (lambda (exn)
                                     (newline)
                                     (display "An exception occurred")
                                     (newline)
                                     (display exn)
                                     (driver-loop))))
                (ambeval input
                         the-global-environment
                         ;; ambeval success
                         (lambda (val next-alternative)
                           (announce-output output-prompt)
                           (user-print val)
                           (internal-loop next-alternative))
                         ;; ambeval failure
                         (lambda ()
                           (announce-output ";;; There are no more values of")
                           (user-print input)
                           (driver-loop)))))))))
  (internal-loop
   (lambda ()
     (newline)
     (display ";;; There is no current problem")
     (driver-loop))))