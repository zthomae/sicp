#lang sicp

(#%require (rename r5rs apply-in-underlying-scheme apply)
           (prefix racket: racket))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (make-machine ops controller-text)
  (let ((machine (make-new-machine)))
    ((machine 'install-operations) ops)
    (let* ((instructions-and-labels (assemble controller-text machine))
           (instructions (car instructions-and-labels))
           (labels (cdr instructions-and-labels)))
      ((machine 'install-instruction-sequence) instructions)
      ((machine 'install-labels) labels)
      machine)))

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

(define (add-unique-value-id lst value)
  (define (loop rest)
    (cond ((null? rest) (cons value lst))
          ((eq? (car rest) value) lst)
          (else (loop (cdr rest)))))
  (loop lst))

(define (filter-id lst x)
  (define (iter acc rest)
    (cond ((null? rest) (reverse acc))
          ((eq? (car rest) x)
           (iter acc (cdr rest)))
          (else (iter (cons x acc) (cdr rest)))))
  (iter '() lst))

(define (contains-id lst x)
  (cond ((null? lst) #f)
        ((eq? (car lst) x) #t)
        (else (contains (cdr lst) x))))

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
        (labels '())
        (entry-points '())
        (stack-registers '())
        (assign-sources '())
        (instruction-count 0)
        (tracing-instructions #f)
        (traced-registers '())
        (active-breakpoints '())
        (continuation #f))
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
        (let ((next-continuation continuation))
          (if next-continuation
              (begin
                (displayln "Resuming execution")
                (set! continuation #f)
                (next-continuation))
              (let ((insts (get-contents pc)))
                (if (null? insts)
                    'done
                    (let* ((next-instruction (car insts))
                           (next-continuation
                             (lambda ()
                               (set! instruction-count (+ instruction-count 1))
                               (if tracing-instructions
                                 (display-instruction next-instruction))
                               ((instruction-execution-proc next-instruction))
                               (execute))))
                      (if (contains-id active-breakpoints next-instruction)
                          (begin
                            (set! continuation next-continuation)
                            (displayln "Paused on breakpoint"))
                          (next-continuation))))))))
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
      (define (find-label label-name)
        (define (iter rest)
          (if (null? rest)
              (error "Could not find label" label-name)
              (let ((next (car rest)))
                (if (equal? label-name (label-entry-name next))
                    next
                    (iter (cdr rest))))))
        (iter labels))
      (define (nth-instruction-after label-name n)
        (list-ref (label-entry-instructions (find-label label-name)) (- n 1)))
      (define (add-breakpoint inst)
        (set! active-breakpoints (add-unique-value-id active-breakpoints inst)))
      (define (remove-breakpoint inst)
        (set! active-breakpoints (filter-id active-breakpoints inst)))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (set! continuation #f)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'install-labels)
               (lambda (seq) (set! labels seq)))
              ((eq? message '_get-instructions)
               (lambda () the-instruction-sequence))
              ((eq? message '_get-labels)
               (lambda () labels))
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
              ((eq? message 'add-breakpoint)
               (lambda (label n) (add-breakpoint (nth-instruction-after label n))))
              ((eq? message 'remove-breakpoint)
               (lambda (label n) (remove-breakpoint (nth-instruction-after label n))))
              ((eq? message 'remove-all-breakpoints)
               (lambda () (set! active-breakpoints '())))
              ((eq? message 'resume-execution)
               (lambda () (execute)))
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
(define (set-breakpoint machine label n)
  ((machine 'add-breakpoint) label n))
(define (clear-breakpoint machine label n)
  ((machine 'remove-breakpoint) label n))
(define (cancel-all-breakpoints machine)
  ((machine 'remove-all-breakpoints)))
(define (proceed-machine machine)
  ((machine 'resume-execution)))

(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))

(define (assemble controller-text machine)
  (extract-labels controller-text
    (lambda (insts labels last-instruction)
      (update-insts! insts labels machine)
      (cons insts labels))))

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
(define (label-entry-name label)
  (car label))
(define (label-entry-instructions label)
  (cdr label))

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
            (displayln "Assigning" register-name "from" (get-contents target) "to" value))
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
  (let* ((register-name (stack-inst-reg-name inst))
         (reg (get-register machine register-name)))
    (lambda ()
      (let ((value (pop stack)))
        (if (tracing-register? machine register-name)
            (displayln "Restoring" register-name "from" (get-contents reg) "to" value))
        (set-contents! reg value)
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

(define recursive-count-leaves-machine
  (make-machine
    (list (list '+ +) (list 'car car) (list 'cdr cdr) (list 'pair? pair?) (list 'null? null?))
    '(start-machine
      (assign continue (label count-leaves-done))
      (assign val (const 0))
      recurse-test
      (test (op null?) (reg tree))
      (branch (label base-case))
      (test (op pair?) (reg tree))
      (branch (label recurse))
      (assign val (const 1))
      (goto (reg continue))
      recurse
      (save continue)
      (assign continue (label after-recurse-1))
      (save tree)
      (assign tree (op car) (reg tree))
      (goto (label recurse-test))
      after-recurse-1
      (assign continue (label after-recurse-2))
      (restore tree)
      (assign tree (op cdr) (reg tree))
      (save val)
      (goto (label recurse-test))
      after-recurse-2
      (restore temp)
      (assign val (op +) (reg val) (reg temp))
      (restore continue)
      (goto (reg continue))
      base-case
      (assign val (const 0))
      (goto (reg continue))
      count-leaves-done)))

(define iterative-count-leaves-machine
  (make-machine
    (list (list '+ +) (list 'car car) (list 'cdr cdr) (list 'pair? pair?) (list 'null? null?))
    '(start-machine
      (assign continue (label count-leaves-done))
      (assign val (const 0))
      (assign n (const 0))
      count-iter
      (test (op null?) (reg tree))
      (branch (label base-case))
      (test (op pair?) (reg tree))
      (branch (label recurse))
      (assign val (op +) (reg n) (const 1))
      (goto (reg continue))
      recurse
      (save continue)
      (assign continue (label after-recurse))
      (save tree)
      (assign tree (op car) (reg tree))
      (goto (label count-iter))
      after-recurse
      (restore tree)
      (assign tree (op cdr) (reg tree))
      (assign n (reg val))
      (restore continue)
      (goto (label count-iter))
      base-case
      (assign val (reg n))
      (goto (reg continue))
      count-leaves-done)))

(define recursive-append-machine
  (make-machine
    (list (list 'null? null?) (list 'car car) (list 'cdr cdr) (list 'cons cons))
    '(start-machine
         (assign continue (label append-done))
       recurse-test
         (test (op null?) (reg x))
         (branch (label base-case))
         (save continue)
         (assign continue (label after-recurse))
         (save x)
         (assign x (op cdr) (reg x))
         (goto (label recurse-test))
       after-recurse
         (restore x)
         (assign rest (reg val))
         (assign first (op car) (reg x))
         (assign val (op cons) (reg first) (reg rest))
         (restore continue)
         (goto (reg continue))
       base-case
         (assign val (reg y))
         (goto (reg continue))
       append-done)))

(define iterative-append-machine
  (make-machine
    (list (list 'null? null?) (list 'cdr cdr) (list 'set-cdr! set-cdr!))
    '(last-pair-init
      (save x)
      last-pair-recurse
      (assign temp (op cdr) (reg x))
      (test (op null?) (reg temp))
      (branch (label after-last-pair-recurse))
      (assign x (reg temp))
      (goto (label last-pair-recurse))
      after-last-pair-recurse
      (assign val (reg x))
      (restore x)
      perform-append
      (perform (op set-cdr!) (reg val) (reg y)))))
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

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

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

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (empty-arglist) '())

(define (adjoin-arg arg arglist)
  (append arglist (list arg)))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (last-operand? ops) (null? (cdr ops)))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

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
        (list 'memq memq)))

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
(define (get-global-environment)
  the-global-environment)

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

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define ece-operations
  (list (list 'self-evaluating? self-evaluating?)
        (list 'variable? variable?)
        (list 'quoted? quoted?)
        (list 'assignment? assignment?)
        (list 'definition? definition?)
        (list 'if? if?)
        (list 'lambda? lambda?)
        (list 'begin? begin?)
        (list 'application? application?)
        (list 'lookup-variable-value lookup-variable-value)
        (list 'text-of-quotation text-of-quotation)
        (list 'lambda-parameters lambda-parameters)
        (list 'lambda-body lambda-body)
        (list 'make-procedure make-procedure)
        (list 'operands operands)
        (list 'operator operator)
        (list 'empty-arglist empty-arglist)
        (list 'no-operands? no-operands?)
        (list 'first-operand first-operand)
        (list 'last-operand? last-operand?)
        (list 'adjoin-arg adjoin-arg)
        (list 'rest-operands rest-operands)
        (list 'primitive-procedure? primitive-procedure?)
        (list 'compound-procedure? compound-procedure?)
        (list 'apply-primitive-procedure apply-primitive-procedure)
        (list 'procedure-parameters procedure-parameters)
        (list 'procedure-environment procedure-environment)
        (list 'extend-environment extend-environment)
        (list 'procedure-body procedure-body)
        (list 'begin-actions begin-actions)
        (list 'first-exp first-exp)
        (list 'last-exp? last-exp?)
        (list 'rest-exps rest-exps)
        (list 'if-predicate if-predicate)
        (list 'true? true?)
        (list 'false? false?)
        (list 'if-alternative if-alternative)
        (list 'if-consequent if-consequent)
        (list 'assignment-variable assignment-variable)
        (list 'assignment-value assignment-value)
        (list 'set-variable-value! set-variable-value!)
        (list 'definition-variable definition-variable)
        (list 'definition-value definition-value)
        (list 'define-variable! define-variable!)
        (list 'cond? cond?)
        (list 'let? let?)
        (list 'let*? let*?)
        (list 'cond->if cond->if)
        (list 'let->combination let->combination)
        (list 'let*->nested-lets let*->nested-lets)
        (list 'prompt-for-input prompt-for-input)
        (list 'read read)
        (list 'get-global-environment get-global-environment)
        (list 'announce-output announce-output)
        (list 'user-print user-print)))

(define eceval
  (make-machine
    ece-operations
    '(read-eval-print-loop
       (perform (op initialize-stack))
       (perform (op prompt-for-input) (const ";;; EC-EVAL input:"))
       (assign exp (op read))
       (assign env (op get-global-environment))
       (assign continue (label print-result))
       (goto (label eval-dispatch))
      eval-dispatch
      (test (op self-evaluating?) (reg exp))
      (branch (label ev-self-eval))
      (test (op variable?) (reg exp))
      (branch (label ev-variable))
      (test (op quoted?) (reg exp))
      (branch (label ev-quoted))
      (test (op assignment?) (reg exp))
      (branch (label ev-assignment))
      (test (op definition?) (reg exp))
      (branch (label ev-definition))
      (test (op if?) (reg exp))
      (branch (label ev-if))
      (test (op lambda?) (reg exp))
      (branch (label ev-lambda))
      (test (op begin?) (reg exp))
      (branch (label ev-begin))
      (test (op cond?) (reg exp))
      (branch (label ev-cond))
      (test (op let?) (reg exp))
      (branch (label ev-let))
      (test (op let*?) (reg exp))
      (branch (label ev-let*))
      (test (op application?) (reg exp))
      (branch (label ev-application))
      (goto (label unknown-expression-type))

      ev-self-eval
      (assign val (reg exp))
      (goto (reg continue))

      ev-variable
      (assign val (op lookup-variable-value) (reg exp) (reg env))
      (goto (reg continue))

      ev-quoted
      (assign val (op text-of-quotation) (reg exp))
      (goto (reg continue))

      ev-lambda
      (assign unev (op lambda-parameters) (reg exp))
      (assign exp (op lambda-body) (reg exp))
      (assign val (op make-procedure) (reg unev) (reg exp) (reg env))
      (goto (reg continue))

      ev-application
      (save continue)
      (save env)
      (assign unev (op operands) (reg exp))
      (save unev)
      (assign exp (op operator) (reg exp))
      (assign continue (label ev-appl-did-operator))
      (goto (label eval-dispatch))

      ev-appl-did-operator
      (restore unev)
      (restore env)
      (assign argl (op empty-arglist))
      (assign proc (reg val))
      (test (op no-operands?) (reg unev))
      (branch (label apply-dispatch))
      (save proc)

      ev-appl-operand-loop
      (save argl)
      (assign exp (op first-operand) (reg unev))
      (test (op last-operand?) (reg unev))
      (branch (label ev-appl-last-arg))
      (save env)
      (save unev)
      (assign continue (label ev-appl-accumulate-arg))
      (goto (label eval-dispatch))

      ev-appl-accumulate-arg
      (restore unev)
      (restore env)
      (restore argl)
      (assign argl (op adjoin-arg) (reg val) (reg argl))
      (assign unev (op rest-operands) (reg unev))
      (goto (label ev-appl-operand-loop))

      ev-appl-last-arg
      (assign continue (label ev-appl-accum-last-arg))
      (goto (label eval-dispatch))

      ev-appl-accum-last-arg
      (restore argl)
      (assign argl (op adjoin-arg) (reg val) (reg argl))
      (restore proc)
      (goto (label apply-dispatch))

      apply-dispatch
      (test (op primitive-procedure?) (reg proc))
      (branch (label primitive-apply))
      (test (op compound-procedure?) (reg proc))
      (branch (label compound-apply))
      (goto (label unknown-procedure-type))

      primitive-apply
      (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
      (restore continue)
      (goto (reg continue))

      compound-apply
      (assign unev (op procedure-parameters) (reg proc))
      (assign env (op procedure-environment) (reg proc))
      (assign env (op extend-environment) (reg unev) (reg argl) (reg env))
      (assign unev (op procedure-body) (reg proc))
      (goto (label ev-sequence))

      ev-begin
      (assign unev (op begin-actions) (reg exp))
      (save continue)
      (goto (label ev-sequence))

      ev-sequence
      (assign exp (op first-exp) (reg unev))
      (test (op last-exp?) (reg unev))
      (branch (label ev-sequence-last-exp))
      (save unev)
      (save env)
      (assign continue (label ev-sequence-continue))
      (goto (label eval-dispatch))

      ev-sequence-continue
      (restore env)
      (restore unev)
      (assign unev (op rest-exps) (reg unev))
      (goto (label ev-sequence))

      ev-sequence-last-exp
      (restore continue)
      (goto (label eval-dispatch))

      ev-if
      (save exp)
      (save env)
      (save continue)
      (assign continue (label ev-if-decide))
      (assign exp (op if-predicate) (reg exp))
      (goto (label eval-dispatch))

      ev-if-decide
      (restore continue)
      (restore env)
      (restore exp)
      (test (op true?) (reg val))
      (branch (label ev-if-consequent))

      ev-if-alternative
      (assign exp (op if-alternative) (reg exp))
      (goto (label eval-dispatch))

      ev-if-consequent
      (assign exp (op if-consequent) (reg exp))
      (goto (label eval-dispatch))

      ev-assignment
      (assign unev (op assignment-variable) (reg exp))
      (save unev)
      (assign exp (op assignment-value) (reg exp))
      (save env)
      (save continue)
      (assign continue (label ev-assignment-1))
      (goto (label eval-dispatch))

      ev-assignment-1
      (restore continue)
      (restore env)
      (restore unev)
      (perform (op set-variable-value!) (reg unev) (reg val) (reg env))
      (assign val (const ok))
      (goto (reg continue))

      ev-definition
      (assign unev (op definition-variable) (reg exp))
      (save unev)
      (assign exp (op definition-value) (reg exp))
      (save env)
      (save continue)
      (assign continue (label ev-definition-1))
      (goto (label eval-dispatch))

      ev-definition-1
      (restore continue)
      (restore env)
      (restore unev)
      (perform (op define-variable!) (reg unev) (reg val) (reg env))
      (assign val (const ok))
      (goto (reg continue))

      ev-cond
      (assign exp (op cond->if) (reg exp))
      (goto (label eval-dispatch))

      ev-let
      (assign exp (op let->combination) (reg exp))
      (goto (label eval-dispatch))

      ev-let*
      (assign exp (op let*->nested-lets) (reg exp))
      (goto (label eval-dispatch))

      print-result
      (perform (op announce-output) (const ";;; EC-Eval value:"))
      (perform (op user-print) (reg val))
      (goto (label read-eval-print-loop))

      unknown-expression-type
      (assign val (const unknown-expression-type-error))
      (goto (label signal-error))

      unknown-procedure-type
      (restore continue) ; clean up stack (from apply-dispatch)
      (assign val (const unknown-procedure-type-error))
      (goto (label signal-error))

      signal-error
      (perform (op user-print) (reg val))
      (goto (label read-eval-print-loop)))))
