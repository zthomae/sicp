#lang sicp

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (make-machine ops controller-text)
  (let ((machine (make-new-machine)))
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

(define (make-register name)
  (let ((contents '*unassigned*))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value) (set! contents value)))
            ((eq? message 'name) name)
            (else
              (error "Unknown request -- REGISTER" message))))
    dispatch))

(define (get-contents register)
  (register 'get))

(define (set-contents! register value)
  ((register 'set) value))

(define (get-register-name register)
  (register 'name))

(define (make-stack)
  (let ((s '())
        (number-pushes 0)
        (max-depth 0)
        (current-depth 0))
    (define (push x)
      (set! s (cons x s))
      (set! number-pushes (+ 1 number-pushes))
      (set! current-depth (+ 1 current-depth))
      (set! max-depth (max current-depth max-depth)))
    (define (pop)
      (if (null? s)
          (error "Empty stack -- POP")
          (let ((top (car s)))
            (set! s (cdr s))
            (set! current-depth (- current-depth 1))
            top)))
    (define (initialize)
      (set! s '())
      (set! number-pushes 0)
      (set! max-depth 0)
      (set! current-depth 0)
      'done)
    (define (print-statistics)
      (newline)
      (display (list 'total-pushes  '= number-pushes
                     'maximum-depth '= max-depth)))
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialize) (initialize))
            ((eq? message 'print-statistics)
             (print-statistics))
            (else
             (error "Unknown request -- STACK" message))))
    dispatch))

(define (pop stack)
  (stack 'pop))

(define (push stack value)
  ((stack 'push) value))

(define (add-unique-value lst value)
  (define (loop rest)
    (cond ((null? rest) (cons value lst))
          ((equal? (car rest) value) lst)
          (else (loop (cdr rest)))))
  (loop lst))

(define (filter lst x)
  (define (iter acc rest)
    (cond ((null? rest) (reverse acc))
          ((equal? (car rest) x)
           (iter acc (cdr rest)))
          (else (iter (cons x acc) (cdr rest)))))
  (iter '() lst))

(define (contains lst x)
  (cond ((null? lst) #f)
        ((equal? (car lst) x) #t)
        (else (contains (cdr lst) x))))

(define (remove-unique-value lst value)
  (filter (lambda (v) (not (equal? value v))) lst))

(define (add-unique-assoc-value lst key value)
  (define (loop prev rest)
    (cond ((null? rest) (cons (list key (list value)) prev))
          ((equal? (caar rest) key)
           (append prev
                   (cons (list key (add-unique-value (cadar rest) value))
                         (cdr rest))))
          (else (loop (cons (car rest) prev) (cdr rest)))))
  (loop '() lst))

(define (insert-between v xs)
  (cond ((null? xs) xs)
        ((null? (cdr xs)) xs)
        (else (cons (car xs)
                    (cons v (insert-between v (cdr xs)))))))

(define (displayln . vs)
  (for-each display (insert-between " " vs))
  (display "\n"))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (instruction-set '())
        (entry-points '())
        (stack-registers '())
        (assign-sources '())
        (instruction-count 0)
        (tracing-instructions #f)
        (traced-registers '()))
    (let ((the-ops
            (list (list 'initialize-stack
                        (lambda () (stack 'initialize)))
                  (list 'print-stack-statistics
                        (lambda () (stack 'print-statistics)))
                  (list 'initialize-instruction-count
                        (lambda () (set! instruction-count 0)))
                  (list 'print-instruction-count
                        (lambda () (displayln "instruction-count =" instruction-count)))))
          (register-table
            (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined reigster: " name)
            (let ((new-register (make-register name)))
              (set! register-table (cons (list name new-register) register-table))
              new-register)))
      (define (find-or-create-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (allocate-register name))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                (set! instruction-count (+ instruction-count 1))
                (if tracing-instructions
                  (display-instruction (car insts)))
                ((instruction-execution-proc (car insts)))
                (execute)))))
      (define (add-instruction inst)
        (set! instruction-set
              (add-unique-assoc-value instruction-set (car inst) inst)))
      (define (add-entry-point destination-register)
        (set! entry-points
              (add-unique-value entry-points destination-register)))
      (define (add-stack-register stack-register-name)
        (set! stack-registers
              (add-unique-value stack-registers stack-register-name)))
      (define (add-assign-source inst)
        (set! assign-sources
              (add-unique-assoc-value assign-sources
                                      (assign-reg-name inst)
                                      (assign-value-exp inst))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'get-register) find-or-create-register)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              ((eq? message 'track-instruction) add-instruction)
              ((eq? message 'get-instruction-set) instruction-set)
              ((eq? message 'track-entry-point) add-entry-point)
              ((eq? message 'get-entry-points) entry-points)
              ((eq? message 'track-stack-register) add-stack-register)
              ((eq? message 'get-stack-registers) stack-registers)
              ((eq? message 'track-assign-source) add-assign-source)
              ((eq? message 'get-assign-sources) assign-sources)
              ((eq? message 'trace-on)
               (lambda () (set! tracing-instructions #t)))
              ((eq? message 'trace-off)
               (lambda () (set! tracing-instructions #f)))
              ((eq? message 'trace-register-on)
               (lambda (register-name)
                (set! traced-registers (add-unique-value traced-registers register-name))))
              ((eq? message 'trace-register-off)
               (lambda (register-name)
                (set! traced-registers (remove-unique-value traced-registers register-name))))
              ((eq? message 'clear-traced-registers)
               (lambda () (set! traced-registers '())))
              ((eq? message 'tracing-register?)
               (lambda (register-name) (contains traced-registers register-name)))
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

(define (start machine)
  (machine 'start))
(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))
(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name) value)
  'done)
(define (trace-on machine)
  ((machine 'trace-on)))
(define (trace-off machine)
  ((machine 'trace-off)))
(define (trace-register-on machine register-name)
  ((machine 'trace-register-on) register-name))
(define (trace-register-off machine register-name)
  ((machine 'trace-register-off) register-name))
(define (clear-traced-registers machine)
  ((machine 'clear-traced-registers)))
(define (tracing-register? machine register-name)
  ((machine 'tracing-register?) register-name))

(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))

(define (assemble controller-text machine)
  (extract-labels controller-text
    (lambda (insts labels last-instruction)
      (update-insts! insts labels machine)
      insts)))

(define (extract-labels text receive)
  (if (null? text)
      (receive '() '() #f)
      (extract-labels (cdr text)
        ;; To process _each result_, we determine whether the next instruction
        ;; is a label or a real instruction, and then process _that instruction_
        ;; correctly by either adding it to the labels or the actual instructions
        (lambda (insts labels last-instruction)
          (let ((next-inst (car text)))
            (if (symbol? next-inst)
                (if (assoc next-inst labels)
                    (error "Duplicate label -- ASSEMBLE" next-inst)
                    (begin
                      (if last-instruction
                          (add-instruction-label! last-instruction next-inst))
                      (receive insts
                               (cons (make-label-entry next-inst insts)
                                     labels)
                               last-instruction)))
                (let ((next-instruction (make-instruction next-inst)))
                  (receive (cons next-instruction insts)
                           labels
                           next-instruction))))))))

(define (track-instruction machine inst)
  ((machine 'track-instruction) inst)
  (cond ((eq? (car inst) 'goto)
         (let ((dest (goto-dest inst)))
           (if (register-exp? dest)
               ((machine 'track-entry-point) (register-exp-reg dest)))))
        ((or (eq? (car inst) 'save) (eq? (car inst) 'restore))
         ((machine 'track-stack-register) (stack-inst-reg-name inst)))
        ((eq? (car inst) 'assign)
         ((machine 'track-assign-source) inst))))

(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations)))
    (for-each
      (lambda (inst)
        (track-instruction machine (instruction-text inst))
        (set-instruction-execution-proc!
          inst
          (make-execution-procedure
            (instruction-text inst) labels machine
            pc flag stack ops)))
      insts)))

(define (make-instruction text)
  (list text '()))
(define (instruction-text inst)
  (car inst))
(define (instruction-labels inst)
  (cadr inst))
(define (instruction-execution-proc inst)
  (cddr inst))
(define (add-instruction-label! inst label)
  (set-cdr! inst (cons (cons label (instruction-labels inst))
                       (instruction-execution-proc inst))))
(define (set-instruction-execution-proc! inst proc)
  (set-cdr! (cdr inst) proc))

(define (display-instruction inst)
  (if (instruction-labels inst)
      (for-each displayln (instruction-labels inst)))
  (displayln "  " (car inst)))

(define (make-label-entry label-name insts)
  (cons label-name insts))

(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
        (cdr val)
        (error "Undefined label -- ASSEMBLE" label-name))))

(define (make-execution-procedure inst labels machine pc flag stack ops)
  (cond ((eq? (car inst) 'assign)
         (make-assign inst machine labels ops pc))
        ((eq? (car inst) 'test)
         (make-test inst machine labels ops flag pc))
        ((eq? (car inst) 'branch)
         (make-branch inst machine labels flag pc))
        ((eq? (car inst) 'goto)
         (make-goto inst machine labels pc))
        ((eq? (car inst) 'save)
         (make-save inst machine stack pc))
        ((eq? (car inst) 'restore)
         (make-restore inst machine stack pc))
        ((eq? (car inst) 'perform)
         (make-perform inst machine labels ops pc))
        (else (error "Unknown instruction type -- ASSEMBLE" inst))))

(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))

(define (make-assign inst machine labels operations pc)
  (let* ((register-name (assign-reg-name inst))
         (target (get-register machine register-name))
         (value-exp (assign-value-exp inst)))
    (let ((value-proc
            (if (operation-exp? value-exp)
                (make-operation-exp value-exp machine labels operations)
                (make-primitive-exp (car value-exp) machine labels))))
      (lambda ()
        (let ((value (value-proc)))
          (if (tracing-register? machine register-name)
            (displayln "Modifying" register-name "from" (get-register-contents machine register-name) "to" value))
          (set-contents! target value)
          (advance-pc pc))))))
(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))
(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))

(define (make-test inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
    (if (operation-exp? condition)
        (let ((condition-proc
                (make-operation-exp condition machine labels operations)))
          (lambda ()
            (set-contents! flag (condition-proc))
            (advance-pc pc)))
        (error "Bad TEST instruction -- ASSEMBLE" inst))))
(define (test-condition test-instruction)
  (cdr test-instruction))

(define (make-branch inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
    (if (label-exp? dest)
        (let ((insts (lookup-label labels (label-exp-label dest))))
          (lambda ()
            (if (get-contents flag)
                (set-contents! pc insts)
                (advance-pc pc))))
        (error "Bad BRANCH instruction -- ASSEMBLE" inst))))
(define (branch-dest branch-instruction)
  (cadr branch-instruction))

(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
    (cond ((label-exp? dest)
           (let ((insts (lookup-label labels (label-exp-label dest))))
             (lambda () (set-contents! pc insts))))
          ((register-exp? dest)
           (let ((reg (get-register machine (register-exp-reg dest))))
             (lambda () (set-contents! pc (get-contents reg)))))
          (else (error "Bad GOTO instruction -- ASSEMBLE" inst)))))
(define (goto-dest goto-instruction)
  (cadr goto-instruction))

(define (make-save inst machine stack pc)
  (let ((reg (get-register machine (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))
(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents! reg (pop stack))
      (advance-pc pc))))
(define (stack-inst-reg-name stack-instruction)
  (cadr stack-instruction))

(define (make-perform inst machine labels operations pc)
  (let ((action (perform-action inst)))
    (if (operation-exp? action)
        (let ((action-proc (make-operation-exp action machine labels operations)))
          (lambda ()
            (action-proc)
            (advance-pc pc)))
        (error "Bad PERFORM instruction -- ASSEMBLE" inst))))
(define (perform-action inst)
  (cdr inst))

(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp)
         (let ((c (constant-exp-value exp)))
           (lambda () c)))
        ((label-exp? exp)
         (let ((insts (lookup-label labels (label-exp-label exp))))
           (lambda () insts)))
        ((register-exp? exp)
         (let ((r (get-register machine (register-exp-reg exp))))
           (lambda () (get-contents r))))
        (else (error "Unknown expression type -- ASSEMBLE" exp))))
(define (register-exp? exp) (tagged-list? exp 'reg))
(define (register-exp-reg exp) (cadr exp))
(define (constant-exp? exp) (tagged-list? exp 'const))
(define (constant-exp-value exp) (cadr exp))
(define (label-exp? exp) (tagged-list? exp 'label))
(define (label-exp-label exp) (cadr exp))

(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
        (aprocs
          (map (lambda (e)
                 (if (label-exp? e)
                     (error "Cannot perform operations on label -- ASSEMBLE" e)
                     (make-primitive-exp e machine labels)))
               (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))
(define (operation-exp? exp)
  (and (pair? exp) (tagged-list? (car exp) 'op)))
(define (operation-exp-op operation-exp)
  (cadr (car operation-exp)))
(define (operation-exp-operands operation-exp)
  (cdr operation-exp))
(define (lookup-prim symbol operations)
  (let ((val (assoc symbol operations)))
    (if val
        (cadr val)
        (error "Unknown operation -- ASSEMBLE" symbol))))

(define test-restore-machine
  (make-machine
    '()
    '(start-machine
      (assign a (const 1))
      (save a)
      (restore b))))

(define recursive-exponentiation-machine
  (make-machine
    (list (list '= =) (list '- -) (list '* *))
    '(start-machine
       (perform (op initialize-instruction-count))
       (assign continue (label expt-done))
       expt-loop
       other-label-expt-loop
       (test (op =) (reg n) (const 0))
       (branch (label base-case))
       (save continue)
       (assign n (op -) (reg n) (const 1))
       (save n)
       (assign continue (label after-expt))
       (goto (label expt-loop))
       after-expt
       (restore n)
       (restore continue)
       (assign val (op *) (reg b) (reg val))
       (goto (reg continue))
       base-case
       (assign val (const 1))
       (goto (reg continue))
       expt-done
       (perform (op print-instruction-count)))))

(define iterative-exponentiation-machine
  (make-machine
    (list (list '= =) (list '- -) (list '* *))
    '(start-machine
       (perform (op initialize-instruction-count))
       (assign counter (reg n))
       (assign product (const 1))
      expt-iter
       (test (op =) (reg counter) (const 0))
       (branch (label after-expt))
       (assign counter (op -) (reg counter) (const 1))
       (assign product (op *) (reg b) (reg product))
       (goto (label expt-iter))
      after-expt
       (assign val (reg product))
       (goto (label expt-done))
      expt-done
       (perform (op print-instruction-count)))))

(define fibonacci-machine
  (make-machine
    (list (list '- -) (list '< <) (list '+ +))
    '(controller
       (assign continue (label fib-done))
       fib-loop
       (test (op <) (reg n) (const 2))
       (branch (label immediate-answer))
       (save continue)
       (assign continue (label afterfib-n-1))
       (save n)                           ; save old value of n
       (assign n (op -) (reg n) (const 1)); clobber n to n - 1
       (goto (label fib-loop))            ; perform recursive call
       afterfib-n-1                         ; upon return, val contains Fib(n - 1)
       (restore n)
       (restore continue)
       (assign n (op -) (reg n) (const 2))
       (save continue)
       (assign continue (label afterfib-n-2))
       (save val)                         ; save Fib(n - 1)
       (goto (label fib-loop))
       afterfib-n-2                         ; upon return, val contains Fib(n - 2)
       (assign n (reg val))               ; n now contains Fib(n - 2)
       (restore val)                      ; val now contains Fib(n - 1)
       (restore continue)
       (assign val                        ;  Fib(n - 1) +  Fib(n - 2)
               (op +) (reg val) (reg n))
       (goto (reg continue))              ; return to caller, answer is in val
       immediate-answer
       (assign val (reg n))               ; base case:  Fib(n) = n
       (goto (reg continue))
       fib-done)))

(define recursive-factorial-machine
  (make-machine
    (list (list '= =) (list '- -) (list '* *))
    '(controller
       (perform (op initialize-stack))
       (assign continue (label fact-done))     ; set up final return address
       fact-loop
       (test (op =) (reg n) (const 1))
       (branch (label base-case))
         ;; Set up for the recursive call by saving n and continue.
         ;; Set up continue so that the computation will continue
         ;; at after-fact when the subroutine returns.
       (save continue)
       (save n)
       (assign n (op -) (reg n) (const 1))
       (assign continue (label after-fact))
       (goto (label fact-loop))
       after-fact
       (restore n)
       (restore continue)
       (assign val (op *) (reg n) (reg val))   ; val now contains n(n - 1)!
       (goto (reg continue))                   ; return to caller
       base-case
       (assign val (const 1))                  ; base case: 1! = 1
       (goto (reg continue))                   ; return to caller
       fact-done
       (perform (op print-stack-statistics)))))
; (define broken-machine
;   (make-machine
;     '(a)
;     '()
;     '(start
;        (goto (label here))
;       here
;        (assign a (const 3))
;        (goto (label there))
;        here
;        (assign a (const 4))
;        (goto (label there))
;        there)))

; (define operating-on-label
;   (make-machine
;     '(a)
;     (list (list '+ +))
;     '(start
;       (assign a (op +) (label start) (const 1)))))
