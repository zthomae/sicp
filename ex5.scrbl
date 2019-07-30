#lang scribble/manual

@require[scribble/lp]
@require[scribble/examples]
@require["eval.rkt"]

@title[#:version "" #:style 'toc]{Chapter 5}

@define[ev @make-eval[]]

@section[#:tag "c5e0"]{Note on diagrams}

I don't think creating diagrams for the data paths is a good
use of my time (they're hard to work with and confuse me
more than help me -- sorry!). This programming note is included
because exercise 5.1 is solely a diagramming question, and I
want the section and exercise numbers to line up.

@section[#:tag "c5e2"]{Exercise 5.2}

This is a literal translation of the iterative factorial
algorithm. The product and counter are both initialized to
@tt{1} at the start, mimicking the initial call to the inner
recursive function. Then, in every iteration, we check
whether the counter is greater than the original input
@tt{n}. If it is, we finish, with the final result being
stored in the @tt{product} register. If it isn't, then we
update the product and increment the counter before starting
again.

Here's an implementation of the given factorial function:

@racketblock[
(controller
  (assign product (const 1))
  (assign counter (const 1))
 test-counter
  (test (op >) (reg counter) (reg n))
  (branch (label factorial-done))
  (assign t (op *) (reg product) (reg counter))
  (assign product (reg t))
  (assign counter (op +) (const 1) (reg counter))
  (goto (label test-counter))
 factorial-done)
 ]

@section[#:tag "c5e3"]{Exercise 5.3}

Consider the following function for computing square roots using
Newton's method:

@racketblock[
(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))
]

We're asked to translate this into a register machine -- first with
the @tt{good-enough?} and @tt{improve} operations presumed to exist
beforehand, and again with the operations defined ourselves.

First, here is a simple machine for computing square roots with these
operations used as primitives.

@racketblock[
(controller
  (assign x (op read))
  (assign guess (const 1.0))
 sqrt-iter
  (test (op good-enough?) (reg guess) (reg x))
  (branch (label sqrt-done))
  (assign guess (op improve) (reg guess) (reg x))
  (goto (label sqrt-iter))
 sqrt-done
  (perform (op print) (reg guess)))
]

Next, here is an expanded version with those functions expanded.

@racketblock[
(controller
  (assign x (op read))
  (assign guess (const 1.0))
 test-guess
  (assign guess-temp (op *) (reg guess) (reg guess))
  (assign guess-temp (op - ) (reg guess-temp) (reg x))
  (assign guess-temp (op abs) (reg guess-temp))
  (test (op <) (reg guess-temp) (const 0.001))
  (branch (label sqrt-done))
 improve-guess
  (assign next-term (op /) (reg x) (reg guess))
  (assign guess (op +) (reg guess) (reg next-term))
  (assign guess (op /) (reg guess) (const 2))
  (goto (label test-guess))
 sqrt-done
  (perform (op print) (reg guess)))
]

@section[#:tag "c5e4"]{Exercise 5.4}

A controller instruction sequence for recursive exponentiation:

@racketblock[
(controller
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
 expt-done)
]

A controller instruction sequence for iterative exponentiation:

@racketblock[
(controller
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
 expt-done)
]

As we've seen before, the recursive version will place vlaues for future
work on the stack, rather than continually channeling them through the
input values of the inner subroutine. However, the cost of doing this
is now more explicit, because you can no longer rely on the implicit call
stack to do this.

@section[#:tag "c5e5"]{Exercise 5.5}

@subsection{Factorial machine}

@racketblock[
(controller
   (assign continue (label fact-done))     ; set up final return address
 fact-loop
   (test (op =) (reg n) (const 1))
   (branch (label base-case))
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
 fact-done)
]

After each instruction, if any register values have changed, they all will be
shown in a table, like this:

@tabular[#:sep @hspace[5]
(list (list @bold{Register} @bold{Value})
      (list "n" "2"))
]

The stack will also be shown when it is updated, like this:

@tabular[
(list (list @bold{Stack value})
      (list "<end>"))
]

@racketblock[
(assign continue (label fact-done))
]

@tabular[#:sep @hspace[5]
(list (list @bold{Register} @bold{Value})
      (list "n" "2")
      (list "continue" "(label fact-done)"))
]

@racketblock[
(test (op =) (reg n) (const 1))
(branch (label base-case))
]

This evaluates to false, so we don't branch.

@racketblock[
(save continue)
(save n)
]

This adds two values to the stack:

@tabular[
(list (list @bold{Stack value})
      (list "2")
      (list "(label fact-done)")
      (list "<end>"))
]

@racketblock[
(assign n (op -) (reg n) (const 1))
(assign continue (label after-fact))
(goto (label fact-loop))
]

This updates the values of @tt{n} and @tt{continue} for the next recursive
call, and jumps to the beginning of the loop.

@tabular[#:sep @hspace[5]
(list (list @bold{Register} @bold{Value})
      (list "n" "1")
      (list "continue" "(label after-fact)"))
]

@racketblock[
(test (op =) (reg n) (const 1))
(branch (label base-case))
]

In this case, we do take this branch, moving to the @tt{base-case} label.

@racketblock[
(assign val (const 1))
(goto (reg continue))
]

@tabular[#:sep @hspace[5]
(list (list @bold{Register} @bold{Value})
      (list "n" "1")
      (list "continue" "(label after-fact)")
      (list "val" "1"))
]

At this point, we have first assigned to the @tt{val} register where the
final result will be stored, and will jump to the instruction currently pointed
at by the @tt{continue} register -- that is, the @tt{after-fact} label.

@racketblock[
(restore n)
(restore continue)
]

@tabular[#:sep @hspace[5]
(list (list @bold{Register} @bold{Value})
      (list "n" "2")
      (list "continue" "(label fact-done)")
      (list "val" "1"))
]

@tabular[
(list (list @bold{Stack value})
      (list "<end>"))
]

@racketblock[
(assign val (op *) (reg n) (reg val))
(goto (reg continue))
]

We assign the final @tt{val} and will presently jump to @tt{fact-done}, ending
the procedure.

@tabular[#:sep @hspace[5]
(list (list @bold{Register} @bold{Value})
      (list "n" "2")
      (list "continue" "(label fact-done)")
      (list "val" "2"))
]

@subsection{Fibonacci machine}

@racketblock[
(controller
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
 fib-done)
]

Our register values will start out like this:

@tabular[#:sep @hspace[5]
(list (list @bold{Register} @bold{Value})
      (list "n" "3"))
]

And our stack is empty:

@tabular[
(list (list @bold{Stack value})
      (list "<end>"))
]

@racketblock[
(assign continue (label fib-done))
]

@tabular[#:sep @hspace[5]
(list (list @bold{Register} @bold{Value})
      (list "n" "3")
      (list "continue" "(label fact-done)"))
]

@racketblock[
(test (op <) (reg n) (const 2))
(branch (label immediate-answer))
]

This is false, so we don't branch.

@racketblock[
(save continue)
]

@tabular[
(list (list @bold{Stack value})
      (list "(label fib-done)")
      (list "<end>"))
]

@racketblock[
(assign continue (label afterfib-n-1))
]

@tabular[#:sep @hspace[5]
(list (list @bold{Register} @bold{Value})
      (list "n" "3")
      (list "continue" "(label afterfib-n-1)"))
]

@racketblock[
(save n)
]

@tabular[
(list (list @bold{Stack value})
      (list "3")
      (list "(label fib-done)")
      (list "<end>"))
]

@racketblock[
(assign n (op -) (reg n) (const 1))
(goto (label fib-loop))
]

We decrement @tt{n} and move on to the next loop.

@tabular[#:sep @hspace[5]
(list (list @bold{Register} @bold{Value})
      (list "n" "2")
      (list "continue" "(label afterfib-n-1)"))
]

@racketblock[
(test (op <) (reg n) (const 2))
(branch (label immediate-answer))
]

This is false, so we don't branch.

@racketblock[
(save continue)
]

@tabular[
(list (list @bold{Stack value})
      (list "(label afterfib-n-1)")
      (list "3")
      (list "(label fib-done)")
      (list "<end>"))
]

@racketblock[
(assign continue (label afterfib-n-1))
]

This happens to do nothing.

@tabular[#:sep @hspace[5]
(list (list @bold{Register} @bold{Value})
      (list "n" "2")
      (list "continue" "(label afterfib-n-1)"))
]

@racketblock[
(save n)
]

@tabular[
(list (list @bold{Stack value})
      (list "2")
      (list "(label afterfib-n-1)")
      (list "3")
      (list "(label fib-done)")
      (list "<end>"))
]

@racketblock[
(assign n (op -) (reg n) (const 1))
(goto (label fib-loop))
]

We decrement @tt{n} and move on to the next loop.

@tabular[#:sep @hspace[5]
(list (list @bold{Register} @bold{Value})
      (list "n" "1")
      (list "continue" "(label afterfib-n-1)"))
]

@racketblock[
(test (op <) (reg n) (const 2))
(branch (label immediate-answer))
]

This is now true, so we will branch.

@racketblock[
(assign val (reg n))
(goto (reg continue))
]

We write the base case value into the @tt{val} register and will presently
jump to @tt{afterfib-n-1}.

@tabular[#:sep @hspace[5]
(list (list @bold{Register} @bold{Value})
      (list "n" "1")
      (list "continue" "(label afterfib-n-1)")
      (list "val" "1"))
]

@racketblock[
(restore n)
(restore continue)
]

@tabular[#:sep @hspace[5]
(list (list @bold{Register} @bold{Value})
      (list "n" "2")
      (list "continue" "(label afterfib-n-1)")
      (list "val" "1"))
]

@tabular[
(list (list @bold{Stack value})
      (list "3")
      (list "(label fib-done)")
      (list "<end>"))
]

@racketblock[
(assign n (op -) (reg n) (const 2))
]

@tabular[#:sep @hspace[5]
(list (list @bold{Register} @bold{Value})
      (list "n" "0")
      (list "continue" "(label afterfib-n-1)")
      (list "val" "1"))
]

@racketblock[
(save continue)
]

@tabular[
(list (list @bold{Stack value})
      (list "(label afterfib-n-1)")
      (list "3")
      (list "(label fib-done)")
      (list "<end>"))
]

@racketblock[
(assign continue (label afterfib-n-2))
]

We prepare to jump into the other recursive branch for the first time.

@tabular[#:sep @hspace[5]
(list (list @bold{Register} @bold{Value})
      (list "n" "0")
      (list "continue" "(label afterfib-n-2)")
      (list "val" "1"))
]

@racketblock[
(save val)
(goto (label fib-loop))
]

We also save the value of Fib(1) -- we're going to need it later.

@tabular[
(list (list @bold{Stack value})
      (list "1")
      (list "(label afterfib-n-1)")
      (list "3")
      (list "(label fib-done)")
      (list "<end>"))
]

After this, we will begin looping again.

@racketblock[
(test (op <) (reg n) (const 2))
(branch (label immediate-answer))
]

This is true, so we branch.

@racketblock[
(assign val (reg n))
(goto (reg continue))
]

We write the base case value into the @tt{val} register (overwriting the value that we
saved to the stack earlier) and will presently jump to @tt{afterfib-n-1}.

@tabular[#:sep @hspace[5]
(list (list @bold{Register} @bold{Value})
      (list "n" "0")
      (list "continue" "(label afterfib-n-2)")
      (list "val" "0"))
]

@racketblock[
(assign n (reg val))
]

This happens to do nothing.

@tabular[#:sep @hspace[5]
(list (list @bold{Register} @bold{Value})
      (list "n" "0")
      (list "continue" "(label afterfib-n-2)")
      (list "val" "0"))
]

@racketblock[
(restore val)
(restore continue)
]

@tabular[#:sep @hspace[5]
(list (list @bold{Register} @bold{Value})
      (list "n" "0")
      (list "continue" "(label afterfib-n-1)")
      (list "val" "1"))
]

@tabular[
(list (list @bold{Stack value})
      (list "3")
      (list "(label fib-done)")
      (list "<end>"))
]

@racketblock[
(assign val (op +) (reg val) (reg n))
(goto (reg continue))
]

This also happens to do nothing. After this, we will jump to @tt{afterfib-n-1}
for another recursive go-around.

@tabular[#:sep @hspace[5]
(list (list @bold{Register} @bold{Value})
      (list "n" "0")
      (list "continue" "(label afterfib-n-1)")
      (list "val" "1"))
]

@racketblock[
(restore n)
(restore continue)
]

@tabular[#:sep @hspace[5]
(list (list @bold{Register} @bold{Value})
      (list "n" "3")
      (list "continue" "(label fib-done)")
      (list "val" "1"))
]

@tabular[
(list (list @bold{Stack value})
      (list "<end>"))
]

@racketblock[
(assign n (op -) (reg n) (const 2))
]

@tabular[#:sep @hspace[5]
(list (list @bold{Register} @bold{Value})
      (list "n" "1")
      (list "continue" "(label fib-done)")
      (list "val" "1"))
]

@racketblock[
(save continue)
]

Notice how we have popped this label off of the stack and immediately put it back.

@tabular[
(list (list @bold{Stack value})
      (list "(label fact-done)")
      (list "<end>"))
]

@racketblock[
(assign continue (label afterfib-n-2))
]

@tabular[#:sep @hspace[5]
(list (list @bold{Register} @bold{Value})
      (list "n" "1")
      (list "continue" "(label afterfib-n-2)")
      (list "val" "1"))
]

@racketblock[
(save val)
(goto (label fib-loop))
]

@tabular[
(list (list @bold{Stack value})
      (list "1")
      (list "(label fact-done)")
      (list "<end>"))
]

@racketblock[
(test (op <) (reg n) (const 2))
(branch (label immediate-answer))
]

This is true, so we will branch.

@racketblock[
(assign val (reg n))
(goto (reg continue))
]

This happens to do nothing.

@tabular[#:sep @hspace[5]
(list (list @bold{Register} @bold{Value})
      (list "n" "1")
      (list "continue" "(label afterfib-n-2)")
      (list "val" "1"))
]

@racketblock[
(assign n (reg val))
]

This happens to do nothing.

@tabular[#:sep @hspace[5]
(list (list @bold{Register} @bold{Value})
      (list "n" "1")
      (list "continue" "(label afterfib-n-2)")
      (list "val" "1"))
]

@racketblock[
(restore val)
(restore continue)
]

@tabular[#:sep @hspace[5]
(list (list @bold{Register} @bold{Value})
      (list "n" "1")
      (list "continue" "(label fact-done)")
      (list "val" "1"))
]

@tabular[
(list (list @bold{Stack value})
      (list "<end>"))
]

@racketblock[
(assign val (op +) (reg val) (reg n))
(goto (reg continue))
]

With this final addition, we reach our final answer and exit the procedure.

@tabular[#:sep @hspace[5]
(list (list @bold{Register} @bold{Value})
      (list "n" "1")
      (list "continue" "(label fact-done)")
      (list "val" "2"))
]

@section[#:tag "c5e6"]{Exercise 5.6}

In the Fibonacci program earlier, @tt{afterfib-n-1} ran the following
instructions in order:

@racketblock[
(restore continue)
(assign n (op -) (reg n) (const 2))
(save continue)
]

Immediately popping a value off of the stack into the @tt{continue} register,
computing a value entirely unrelated to it, and then pushing the value in the
@tt{continue} register back onto the stack is obviously an unnecessary
@tt{restore}/@tt{save} pair. Both of these instructions can be deleted without
consequence.

@section[#:tag "c5e7"]{Exercise 5.7}

We can simulate the recursive and iterative exponentiation machines by wrapping
them in @tt{make-machine} declarations. This can be done mechanically.

First, the recursive machine:

@racketblock[
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
]

Verifying that it works:

@verbatim{
> (set-register-contents! recursive-exponentiation-machine 'b 3)
'done
> (set-register-contents! recursive-exponentiation-machine 'n 10)
'done
> (start recursive-exponentiation-machine)
'done
> (get-register-contents recursive-exponentiation-machine 'val)
59049
}

Next, the iterative machine:

@racketblock[
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
]

Verifying that it also works:

@verbatim{
> (set-register-contents! iterative-exponentiation-machine 'b 3)
'done
> (set-register-contents! iterative-exponentiation-machine 'n 10)
'done
> (start iterative-exponentiation-machine )
'done
> (get-register-contents iterative-exponentiation-machine 'val)
59049
}

Of course, if we use the builtin @tt{expt} function, we can see that this
result is correct:

@verbatim{
> (expt 3 10)
59049
}

@section[#:tag "c5e8"]{Exercise 5.8}

Consider the following program with an ambiguous label:

@racketblock[
start
  (goto (label here))
here
  (assign a (const 3))
  (goto (label there))
here
  (assign a (const 4))
  (goto (label there))
there
]

We can see in @tt{extract-labels} that new labels are prepended to the
list of existing labels. However, the instructions are iterated through in
reverse order -- the fact that @tt{extract-labels} calls @tt{(extract-labels (cdr text) ...)} is
a tell-tale clue of this. This means that the first declaration of the @tt{here}
label will be first in the list. Since @tt{lookup-label} uses @tt{assoc}, it will
therefore return the @italic{first} instance of the @tt{here} label in code.

We can verify this by simulation:

@racketblock[
(define broken-machine
  (make-machine
    '(a)
    '()
    '(start
       (goto (label here))
      here
       (assign a (const 3))
       (goto (label there))
      here
       (assign a (const 4))
       (goto (label there))
      there)))
]

@verbatim{
> (start broken-machine )
'done
> (get-register-contents broken-machine 'a)
3
}

We can fix this by forcing @tt{extract-labels} to verify that a label with
a given name doesn't already exist in the list of labels. It's not very
efficient, but @tt{assoc} will do the job here with minimal changes:

@racketblock[
(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels (cdr text)
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
]

@section[#:tag "c5e9"]{Exercise 5.9}

Currently, the machine does not reject operations that are performed on
labels. For example, this example program is assembled correctly:

@racketblock[
(define operating-on-label
  (make-machine
    '(a)
    (list (list '+ +))
    '(start
      (assign a (op +) (label start) (const 1)))))
]

Naturally, this program doesn't actually work -- instead, it throws a
runtime error when trying to apply the @tt{+} operation.

@verbatim{
> (start operating-on-label )
; +: contract violation
;   expected: number?
;   given: (mcons (mcons (mcons 'assign (mcons 'a (mcons (mcons 'op (mcons '+
;     '())) (mcons (mcons 'label (mcons 'start '())) (mcons (mcons 'const
;     (mcons 1 '())) '()))))) #<procedure:...ch/5/machine.rkt:198:6>) '())
;   argument position: 1st
}

Since performing operations on labels is (probably) not a good idea, we can
revise the machine to reject these programs. All that we need to do is insert
a check in @tt{make-operation-exp} to verify that primitive expressions are not
labels:

@racketblock[
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
]

It's a little ugly, but I think this is a reasonable place to check the expression
type. Obviously we want to check the expression itself rather than the expression
procedure, so it's convenient to do so in a location where each operand expression
is already in scope. And we don't want to push down any new logic into
@tt{make-primitive-exp}, because labels @italic{are} primitives. As an alternative,
you might also consider reporting on @italic{all} of the operands that are labels.

For now, we can verify that what we've written rejects the program above:

@verbatim{
Cannot perform operations on label -- ASSEMBLE {label start}
}

@section[#:tag "c5e10"]{Exercise 5.10}

There are a couple of areas in the syntax where I think readability can be
improved by relying on a few more conventions and assumptions.

For example: The explicit declaration of @tt{reg}, @tt{label}, and @tt{const}
expressions is kind of tiresome. We can make this simpler by distinguishing
these by the forms of the expressions themselves:

@itemlist[
@item{The only primitives that the interpreter will support are numbers, strings,
lists, and symbols. The first three can all be unambiguously represented literally,
and constant symbols can be differentiated by forcing them to begin with a leading quote.}
@item{We can easily distinguish registers by forcing them to start with a special character,
like $.}
@item{If we have a symbol that doesn't start with a $, then we have a label.}
]

We could take this even further and drop some of these requirements where we don't need
to distinguish between otherwise-identical expressions. For example, including leading
@tt{$}s for the first arguments to @tt{assign} expressions is pure noise, because
we can @italic{only} assign to registers (in fact, it could be optional, to prevent
people from making the opposite mistake).

Not all syntactic changes will be, but these @italic{can} be implemented solely by
changing the relevant type-checking and value-accessing functions for the types
of expressions we want to change. The entirety of the changes are presented below:

@racketblock[
(define (assign-reg-name assign-instruction)
  (let ((exp (cadr assign-instruction)))
    (if (register-exp? exp)
        (register-exp-reg exp)
        exp)))

(define (symbol-starts-with? sym c)
  (equal? c (string-ref (symbol->string sym) 0)))

(define (register-exp? exp)
  (and (symbol? exp) (symbol-starts-with? exp #\$)))
(define (register-exp-reg exp)
  (string->symbol (substring (symbol->string exp) 1)))

(define (constant-exp? exp)
  (or (number? exp)
      (string? exp)
      (and (list? exp) (not (operation-exp? exp)))))
(define (constant-exp-value exp)
  (if (and (list? exp) (> (length exp) 0) (eq? (car exp) 'quote))
      (cadr exp)
      exp))

(define (label-exp? exp)
  (and (symbol? exp) (not (symbol-starts-with? exp #\$))))
(define (label-exp-label exp)
  exp)
]

Two example programs are presented below:

@racketblock[
(define test-machine
  (make-machine
    '(a b c d e)
    (list (list '+ +))
    '(start
       (assign a 1)
       (assign b here)
       (assign c "asdf")
       (assign d '(1 2 3 4))
       (assign e 'asdf)
       (goto $b)
      here
       (assign $a (op +) $a 1)
       (goto there)
      there)))
]

@verbatim{
> (start test-machine)
'done
> (get-register-contents test-machine 'a)
2
> (get-register-contents test-machine 'b)
(mcons
 (mcons
  (mcons
   'assign
   (mcons '$a (mcons (mcons 'op (mcons '+ '())) (mcons '$a (mcons 1 '())))))
  #<procedure:.../machine-alt.rkt:199:6>)
 (mcons
  (mcons
   (mcons 'goto (mcons 'there '()))
   #<procedure:.../machine-alt.rkt:238:13>)
  '()))
> (get-register-contents test-machine 'c)
"asdf"
> (get-register-contents test-machine 'd)
(mcons 1 (mcons 2 (mcons 3 (mcons 4 '()))))
> (get-register-contents test-machine 'e)
'asdf
}

@racketblock[
(define iterative-exponentiation-machine
  (make-machine
    '(b n val counter product)
    (list (list '= =) (list '- -) (list '* *))
    '(start-machine
       (assign counter $n)
       (assign product 1)
      expt-iter
       (test (op =) $counter 0)
       (branch after-expt)
       (assign counter (op -) $counter 1)
       (assign product (op *) $b $product)
       (goto expt-iter)
      after-expt
       (assign $val $product)
       (goto expt-done)
      expt-done)))
]

@verbatim{
> (set-register-contents! iterative-exponentiation-machine 'b 3)
'done
> (set-register-contents! iterative-exponentiation-machine 'n 10)
'done
> (start iterative-exponentiation-machine)
'done
> (get-register-contents iterative-exponentiation-machine 'val)
59049
}

It's a small thing, but I think we're already on the road to slightly more
legible programs.

@section[#:tag "c5e11"]{Exercise 5.11}

There are (apparently) three different meanings of @tt{restore}. Paraphrasing
the book:

@itemlist[
@item{It puts the last value on the stack into the provided register, regardless of what register it came from.}
@item{It puts the last value on the stack into the provided register, failing if that value wasn't saved
from that register originally.}
@item{It puts the last value saved onto the stack from the provided register back into it (essentially
maintaining one stack per register).}
]

Full disclosure: I believe that the first meaning, which is currently implemented in the
interpreter, is by far the most reasonable one. But that might be because I'm used to it.

Consider the following instructions from the Fibonacci machine:

@racketblock[
afterfib-n-2
  (assign n (reg val))
  (restore val)
  (restore continue)
  (assign val (op +) (reg val) (reg n))
  (goto (reg continue))
]

The @tt{+} operation will continue to work as long as @tt{val} and @tt{n}
each contain one of the two values we need -- after all, addition is
commutative. Observe how we move the value from the @tt{val} register into
@tt{n} and pop the latest stack value into @tt{val} right after. What would have
happened if we had run @tt{(restore n)} instead? The value that had been in
@tt{val} at the start would have stayed there, and the value that was restored into
@tt{val} would instead be in @tt{n}. We would add the same values and place the
result into @tt{val}, so the program would continue to work as it should.

However, we could modify the interpreter to support the other meanings of @tt{restore}.
The second, which verifies that you @tt{restore} into the same register from which
a value was @tt{save}d, can be done relatively easily. All that needs to happen is for
the register from which a value originated to be saved on the stack alongside the
value. If @tt{restore} is called and the registers don't match, the interpreter
should throw an error. The changes can almost entirely be confined to @tt{make-save} and
@tt{make-restore}:

@racketblock[
(define (make-save inst machine stack pc)
  (let ((reg (get-register machine (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (cons (get-register-name reg) (get-contents reg)))
      (advance-pc pc))))
(define (make-restore inst machine stack pc)
  (let* ((to-register (stack-inst-reg-name inst))
         (reg (get-register machine to-register)))
    (lambda ()
      (let* ((stack-entry (pop stack))
             (from-register (car stack-entry))
             (stack-value (cdr stack-entry)))
        (if (eq? from-register to-register)
            (begin
              (set-contents! reg stack-value)
              (advance-pc pc))
            (error (string-append "Tried to restore from register "
                                  (symbol->string from-register)
                                  " into register "
                                  (symbol->string to-register)
                                  " -- ASSEMBLE")))))))
]

To support this, I've added a new operation on registers to retrieve the name:

@racketblock[
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

(define (get-register-name register)
  (register 'name))
]

We can verify that this works with the following test program:

@racketblock[
(define test-restore-machine
  (make-machine
    '(a b)
    '()
    '(start-machine
      (assign a (const 1))
      (save a)
      (restore b))))
]

@verbatim{
> (start test-restore-machine)
; Tried to restore from register a into register b -- ASSEMBLE
}

To support the third meaning, where each register has an independent stack,
we will need to make larger changes to the interpreter. For one, @tt{make-stack}
is now going to be a bit of a misnomer -- instead, this should create stacks for each
of the registers to use. However, for clarity's sake, I'll be leaving the name
of this function alone.

We are told that @tt{initialize-stack} should initialize all of the register stacks.
This is a little bit interesting -- @tt{initialize-stack} is technically
unused as of yet in the interpreter -- but it does suggest something about how
the design should work. As of now, the machine doesn't have a dedicated list
of all of its registers available when the stack is created, because the user-specified
register list is only used after this happens. However, the function that calls
@tt{make-new-machine} @italic{does} know about these. This implies that we need
to move our initialization steps around a bit.

The essential change that I'll make is to pass the register names in directly
to @tt{make-new-machine}, and to make this function responsible for initializing
the registers as well as the stacks. Then I'll create an list of pairs of register
names combined with their stacks. To save from or restore into a given register, I'll
find the stack by name and then perform the normal stack operations on that.

All of the modified functions are below. Note that the operation for creating
registers is now fully private.

@racketblock[
(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine register-names)))
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

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
]

There were also a couple of places where I renamed a @tt{stack} variable or operation to
@tt{stacks}, to be more suggestive of its new value. Otherwise, these functions
are identical.

@racketblock[
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
]

Now we can run a modified version of the program given above and see that
the stacks are independent:

@racketblock[
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
]

@verbatim{
> (start test-restore-machine)
'done
> (get-register-contents test-restore-machine 'a)
1
> (get-register-contents test-restore-machine 'b)
2
}
