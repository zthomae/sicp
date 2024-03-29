#lang sicp

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine register-names)))
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
  (let ((s '()))
    (define (push x)
      (set! s (cons x s)))
    (define (pop)
      (if (null? s)
          (error "Empty stack -- POP")
          (let ((top (car s)))
            (set! s (cdr s))
            top)))
    (define (initialize)
      (set! s '())
      'done)
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialize) (initialize))
            (else (error "Unknown request -- STACK" message))))
    dispatch))

(define (pop stack)
  (stack 'pop))

(define (push stack value)
  ((stack 'push) value))

(define (make-new-machine register-names)
  (let* ((pc (make-register 'pc))
         (flag (make-register 'flag))
         (all-register-names (cons 'pc (cons 'flag register-names)))
         (stacks (map (lambda (register-name) (cons register-name (make-stack)))
                      all-register-names))
         (the-instruction-sequence '()))
    (let ((the-ops
            (list (list 'initialize-stack (lambda () (map (lambda (stack) (stack 'initialize))
                                                          stacks)))))
          (register-table
            (list (list 'pc pc) (list 'flag flag))))
      (for-each (lambda (name)
                  (if (assoc name register-table)
                     (error "Multiply defined register: " name)
                     (set! register-table
                           (cons (list name (make-register name))
                                 register-table))))
                register-names)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (error "Unknown register:" name))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                ((instruction-execution-proc (car insts)))
                (execute)))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stacks) stacks)
              ((eq? message 'operations) the-ops)
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

(define (start machine)
  (machine 'start))
(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))
(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name) value)
  'done)

(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))

(define (assemble controller-text machine)
  (extract-labels controller-text
    (lambda (insts labels)
      (update-insts! insts labels machine)
      insts)))

(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels (cdr text)
        ;; To process _each result_, we determine whether the next instruction
        ;; is a label or a real instruction, and then process _that instruction_
        ;; correctly by either adding it to the labels or the actual instructions
        (lambda (insts labels)
          (let ((next-inst (car text)))
            (if (symbol? next-inst)
                (if (assoc next-inst labels)
                    (error "Duplicate label -- ASSEMBLE" next-inst)
                    (receive insts
                             (cons (make-label-entry next-inst insts)
                                   labels)))
                (receive (cons (make-instruction next-inst)
                               insts)
                         labels)))))))

(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stacks (machine 'stacks))
        (ops (machine 'operations)))
    (for-each
      (lambda (inst)
        (set-instruction-execution-proc!
          inst
          (make-execution-procedure
            (instruction-text inst) labels machine
            pc flag stacks ops)))
      insts)))

(define (make-instruction text)
  (cons text '()))
(define (instruction-text inst)
  (car inst))
(define (instruction-execution-proc inst)
  (cdr inst))
(define (set-instruction-execution-proc! inst proc)
  (set-cdr! inst proc))

(define (make-label-entry label-name insts)
  (cons label-name insts))

(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
        (cdr val)
        (error "Undefined label -- ASSEMBLE" label-name))))

(define (make-execution-procedure inst labels machine pc flag stacks ops)
  (cond ((eq? (car inst) 'assign)
         (make-assign inst machine labels ops pc))
        ((eq? (car inst) 'test)
         (make-test inst machine labels ops flag pc))
        ((eq? (car inst) 'branch)
         (make-branch inst machine labels flag pc))
        ((eq? (car inst) 'goto)
         (make-goto inst machine labels pc))
        ((eq? (car inst) 'save)
         (make-save inst machine stacks pc))
        ((eq? (car inst) 'restore)
         (make-restore inst machine stacks pc))
        ((eq? (car inst) 'perform)
         (make-perform inst machine labels ops pc))
        (else (error "Unknown instruction type -- ASSEMBLE" inst))))

(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))

(define (make-assign inst machine labels operations pc)
  (let ((target (get-register machine (assign-reg-name inst)))
        (value-exp (assign-value-exp inst)))
    (let ((value-proc
            (if (operation-exp? value-exp)
                (make-operation-exp value-exp machine labels operations)
                (make-primitive-exp (car value-exp) machine labels))))
      (lambda ()
        (set-contents! target (value-proc))
        (advance-pc pc)))))
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

(define (make-save inst machine stacks pc)
  (let* ((register-name (stack-inst-reg-name inst))
         (stack (cdr (assoc register-name stacks)))
         (reg (get-register machine register-name)))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))
(define (make-restore inst machine stacks pc)
  (let* ((register-name (stack-inst-reg-name inst))
         (reg (get-register machine register-name)))
    (lambda ()
      (let ((stack (cdr (assoc register-name stacks))))
        (set-contents! reg (pop stack))
        (advance-pc pc)))))
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
    '(a b)
    '()
    '(start-machine
      (assign a (const 1))
      (assign b (const 2))
      (save a)
      (save b)
      (restore b)
      (restore a))))

(define recursive-exponentiation-machine
  (make-machine
    '(b n continue val)
    (list (list '= =) (list '- -) (list '* *))
    '(start-machine
       (assign continue (label expt-done))
       expt-loop
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
       expt-done)))

(define iterative-exponentiation-machine
  (make-machine
    '(b n val counter product)
    (list (list '= =) (list '- -) (list '* *))
    '(start-machine
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
      expt-done)))

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
