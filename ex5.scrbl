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
  (let* ((register-name (stack-inst-reg-name inst))
         (reg (get-register machine register-name)))
    (lambda ()
      (push stack (cons register-name (get-contents reg)))
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

@section[#:tag "c5e12"]{Exercise 5.12}

We are asked to add four pieces of tracking to the interpreter, to show the
data paths required when building it. They are:

@itemlist[
@item{A list of all unique instructions sorted by instruction type}
@item{A list of all registers used in goto instructions}
@item{A list of all registers used in save or restore instructions}
@item{For each register, a list of all expressions that are assigned to it}
]

Since these all rely on maintaining lists of unique values, creating procedures
for these operations is a good place to start. One, @tt{add-unique-value},
will return the original list if a value already exists in it, and @tt{cons}
the new value onto the front otherwise. The other, @tt{add-unique-assoc-value},
will use @tt{add-unique-value} to add to a list within an associative array.
If the key already exists in the array, it will return a new associative array
with that key bound to the expanded list. If the key does not exist, it will
add it to the front. These functions are all immutable, and they are relatively
efficient because I assume that ordering within these lists is not important.

@racketblock[
(define (add-unique-value lst value)
  (define (loop rest)
    (cond ((null? rest) (cons value lst))
          ((equal? (car rest) value) lst)
          (else (loop (cdr rest)))))
  (loop lst))

(define (add-unique-assoc-value lst key value)
  (define (loop prev rest)
    (cond ((null? rest) (cons (list key (list value)) prev))
          ((equal? (caar rest) key)
           (append prev
                   (cons (list key (add-unique-value (cadar rest) value))
                         (cdr rest))))
          (else (loop (cons (car rest) prev) (cdr rest)))))
  (loop '() lst))
]

Next, I have to maintain lists of instructions, entry points, stack registers,
and assign sources within the internal machine state. This is relatively
easy to do -- all I need are more private bindings that I can manipulate by
sending new messages to the machine. The modified @tt{make-new-machine} is as
follows:

@racketblock[
(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (instruction-set '())
        (entry-points '())
        (stack-registers '())
        (assign-sources '()))
    (let ((the-ops
            (list (list 'initialize-stack (lambda () (stack 'initialize)))))
          (register-table
            (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined reigster: " name)
            (set! register-table
                  (cons (list name (make-register name))
                        register-table)))
        'register-allocated)
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
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
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
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))
]

Finally, I need to call these tracking instructions as I'm adding instructions
to the machine. I've combined the checks for all things I might need to track
to a single function, so as to reduce the scope and messiness of the changes
to the existing interpreter code. The new method, @tt{track-instruction}, and
the modified @tt{update-insts!} that calls it are:

@racketblock[
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
]

With this, we can construct the Fibonacci machine and see what it finds:

@verbatim{
> (display (fibonacci-machine 'get-instruction-set))
((branch ((branch (label immediate-answer))))
 (assign ((assign val (reg n))
          (assign val (op +) (reg val) (reg n))
          (assign n (reg val))
          (assign continue (label afterfib-n-2))
          (assign n (op -) (reg n) (const 2))
          (assign n (op -) (reg n) (const 1))
          (assign continue (label afterfib-n-1))
          (assign continue (label fib-done))))
 (restore ((restore val)
           (restore continue)
           (restore n)))
 (save ((save val)
        (save n)
        (save continue)))
 (test ((test (op <) (reg n) (const 2))))
 (goto ((goto (reg continue))
        (goto (label fib-loop)))))

> (display (fibonacci-machine 'get-entry-points))
(continue)

> (display (fibonacci-machine 'get-stack-registers))
(val n continue)

> (display (fibonacci-machine 'get-assign-sources))
((val (((reg n))
       ((op +) (reg val) (reg n))))
 (continue (((label afterfib-n-2))
            ((label afterfib-n-1))
            ((label fib-done))))
 (n (((reg val))
     ((op -) (reg n) (const 2))
     ((op -) (reg n) (const 1)))))
}

@section[#:tag "c5e13"]{Exercise 5.13}

I've been waiting to change the simular to automatically create the
registers it needs as it assembles the instructions -- passing in a list
manually is a tiresome friction when putting together programs. (In real life,
of course, the registers are fixed, but our machine doesn't work like that.)

In principle, we need to change every instruction that deals with registers
to allow the registers to be created first. Of course, actually @tt{changing}
every instruction like this would be wildly unsustainable. Instead, I would
like to change the @tt{get-register} operation so it's responsible for creating
registers if they don't already exist. That way, the assembly process can be
entirely agnostic as to whether the instructions are preallocated or created
on the fly.

Starting at the top-level, we change @tt{make-machine} so that it no longer
accepts a list of registers and no longer explicitly creates them. This seems
like an improvement anyway:

@racketblock[
(define (make-machine ops controller-text)
  (let ((machine (make-new-machine)))
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))
]

Next, in @tt{make-new-machine} we replace the @tt{lookup-register} operation
with the more aptly-named @tt{find-or-create-register}. This does almost the
same thing, except in the case that the register doesn't exist it calls
@tt{allocate-register} to create it. @tt{allocate-register} is then changed
to return the new register it created, in addition to adding it to the register
table, and is removed from the public interface of the machine.

@racketblock[
(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (instruction-set '())
        (entry-points '())
        (stack-registers '())
        (assign-sources '()))
    (let ((the-ops
            (list (list 'initialize-stack (lambda () (stack 'initialize)))))
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
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))
]

And that's all we have to do. We can now define machines without passing in lists
of registers and they will work normally:

@racketblock[
(define iterative-exponentiation-machine
  (make-machine
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

@section[#:tag "c5e14"]{Exercise 5.14}

We are asked to run the recursive factorial program through the interpreter
with stack statistics enabled, deriving a formula for the relationship between
@tt{n} and the number of stack operations and the maximum depth of the stack
during the execution of the program.

We should expect to see a linear relationship between both of these and the input
size, because the program recurses by repeatedly subtracting one from the input.
Let's verify that.

First, here's an instrumented version of the factorial program that initializes
the stack at the beginning of the program and prints the stack statistics
at the end:

@racketblock[
(define recursive-factorial-machine
  (make-machine
    (list (list '= =) (list '- -) (list '* *))
    '(controller
       (perform (op initialize-stack))
       (assign continue (label fact-done))
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
       (assign val (op *) (reg n) (reg val))
       (goto (reg continue))
       base-case
       (assign val (const 1))
       (goto (reg continue))
       fact-done
       (perform (op print-stack-statistics)))))
]

Next, let's run the machine for the integers from @tt{1} to @tt{30}:

@racketblock[
(define (build-list n proc)
  (define (acc vals next)
    (if (> 0 next)
        vals
        (acc (cons (proc next) vals) (- next 1))))
  (acc '() (- n 1)))
]

@verbatim{
> (for-each
    (lambda (n)
      (set-register-contents! recursive-factorial-machine 'n n)
      (start recursive-factorial-machine))
    (build-list 30 (lambda (i) (+ i 1))))

(total-pushes = 0 maximum-depth = 0)
(total-pushes = 2 maximum-depth = 2)
(total-pushes = 4 maximum-depth = 4)
(total-pushes = 6 maximum-depth = 6)
(total-pushes = 8 maximum-depth = 8)
(total-pushes = 10 maximum-depth = 10)
(total-pushes = 12 maximum-depth = 12)
(total-pushes = 14 maximum-depth = 14)
(total-pushes = 16 maximum-depth = 16)
(total-pushes = 18 maximum-depth = 18)
(total-pushes = 20 maximum-depth = 20)
(total-pushes = 22 maximum-depth = 22)
(total-pushes = 24 maximum-depth = 24)
(total-pushes = 26 maximum-depth = 26)
(total-pushes = 28 maximum-depth = 28)
(total-pushes = 30 maximum-depth = 30)
(total-pushes = 32 maximum-depth = 32)
(total-pushes = 34 maximum-depth = 34)
(total-pushes = 36 maximum-depth = 36)
(total-pushes = 38 maximum-depth = 38)
(total-pushes = 40 maximum-depth = 40)
(total-pushes = 42 maximum-depth = 42)
(total-pushes = 44 maximum-depth = 44)
(total-pushes = 46 maximum-depth = 46)
(total-pushes = 48 maximum-depth = 48)
(total-pushes = 50 maximum-depth = 50)
(total-pushes = 52 maximum-depth = 52)
(total-pushes = 54 maximum-depth = 54)
(total-pushes = 56 maximum-depth = 56)
(total-pushes = 58 maximum-depth = 58)
}

As predicted, there is a strict linear relationship between the number
of pushes (and the maximum depth of the stack) and @tt{n} -- in this case,
with the number of pushes and the maximum stack depth being @tt{2 * (n - 1)}.

@section[#:tag "c5e15"]{Exercise 5.15}

Tracking the number of instructions that have been executed is very easy.

First, we add a new piece of mutable state within the machine to keep track of the
number of instructions that have been executed, and operations for clearing/printing
this value:

@racketblock[
(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (instruction-set '())
        (entry-points '())
        (stack-registers '())
        (assign-sources '())
        (instruction-count 0))
    (let ((the-ops
            (list (list 'initialize-stack
                        (lambda () (stack 'initialize)))
                  (list 'print-stack-statistics
                        (lambda () (stack 'print-statistics)))
                  (list 'initialize-instruction-count
                        (lambda () (set! instruction-count 0)))
                  (list 'print-instruction-count
                        (lambda () (displayln "instruction-count =" instruction-count)))))
          ;; ...
          ))))
]

Next, we modify the internal @tt{execute} procedure so it increments this counter
before executing each instruction:

@racketblock[
(define (execute)
  (let ((insts (get-contents pc)))
    (if (null? insts)
        'done
        (begin
          (set! instruction-count (+ instruction-count 1))
          ((instruction-execution-proc (car insts)))
          (execute)))))
]

Lastly, we initialize this counter to 0 before our program starts and print it
as it exists. For convenience, we can do that by adding these instructions
to the beginning/end of the program as we did in the previous exercise.

As an example, let's instrument the recursive and iterative exponentiation
programs and see how they compare. First, the recursive machine:

@racketblock[
(define recursive-exponentiation-machine
  (make-machine
    (list (list '= =) (list '- -) (list '* *))
    '(start-machine
       (perform (op initialize-instruction-count))
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
      expt-done
       (perform (op print-instruction-count)))))
]

Next, the iterative machine:

@racketblock[
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
]

And running them both with the same inputs:

@verbatim{
> (set-register-contents! recursive-exponentiation-machine 'b 2)
'done
> (set-register-contents! recursive-exponentiation-machine 'n 10)
'done
> (start recursive-exponentiation-machine)
instruction-count = 116
'done

> (set-register-contents! iterative-exponentiation-machine 'b 2)
'done
> (set-register-contents! iterative-exponentiation-machine 'n 10)
'done
> (start iterative-exponentiation-machine)
instruction-count = 57
'done
}

As we expected, the iterative machine is significantly efficient than the
recursive machine in that it executes many fewer instructions.

@section[#:tag "c5e16"]{Exercise 5.16}

To add instruction tracing, we can instrument the internal @tt{execute} procedure
in the machine to output instructions before executing them. I think this is
better than instrumenting the instruction procedures themselves because
the changes to allow instruction tracing are guaranteed to stay isolated here.
It also seems right to me to separate the actual operations that each instruction
performs from a machine-level concern like instruction tracing.

The exercise doesn't specify how the instructions ought to be displayed,
so we're going to do the simple thing and print the forms literally with no
extra formatting applied. The @tt{displayln} helper function I wrote awhile
ago will make this almost completely trivial. It's also trivial to add commands
to turn instruction tracing on and off.

In @tt{make-new-machine}, we modify the following segments. The rest has been
omitted for clarity.

@racketblock[
(let ((pc (make-register 'pc))
      (flag (make-register 'flag))
      (stack (make-stack))
      (the-instruction-sequence '())
      (instruction-set '())
      (entry-points '())
      (stack-registers '())
      (assign-sources '())
      (instruction-count 0)
      (tracing-instructions #f))
  ...

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
  ...

  (define (dispatch message)
    (cond ...
      ((eq? message 'trace-on)
       (lambda () (set! tracing-instructions #t)))
      ((eq? message 'trace-off)
       (lambda () (set! tracing-instructions #f))))))
]

To support this, we use a few helper functions:

@racketblock[
(define (trace-on machine)
  ((machine 'trace-on)))
(define (trace-off machine)
  ((machine 'trace-off)))

(define (display-instruction inst)
  (displayln (car inst)))
]

And with that, we're all done:

@verbatim{
> (set-register-contents! recursive-exponentiation-machine 'b 2)
'done

> (set-register-contents! recursive-exponentiation-machine 'n 2)
'done

> (trace-on recursive-exponentiation-machine)

> (start recursive-exponentiation-machine)
(perform (op initialize-instruction-count))
(assign continue (label expt-done))
(test (op =) (reg n) (const 0))
(branch (label base-case))
(save continue)
(assign n (op -) (reg n) (const 1))
(save n)
(assign continue (label after-expt))
(goto (label expt-loop))
(test (op =) (reg n) (const 0))
(branch (label base-case))
(save continue)
(assign n (op -) (reg n) (const 1))
(save n)
(assign continue (label after-expt))
(goto (label expt-loop))
(test (op =) (reg n) (const 0))
(branch (label base-case))
(assign val (const 1))
(goto (reg continue))
(restore n)
(restore continue)
(assign val (op *) (reg b) (reg val))
(goto (reg continue))
(restore n)
(restore continue)
(assign val (op *) (reg b) (reg val))
(goto (reg continue))
(perform (op print-instruction-count))
instruction-count = 28
'done

> (get-register-contents recursive-exponentiation-machine 'val)
4

> (set-register-contents! recursive-exponentiation-machine 'b 2)
'done

> (set-register-contents! recursive-exponentiation-machine 'n 2)
'done

> (trace-off recursive-exponentiation-machine)

> (start recursive-exponentiation-machine)
instruction-count = 28
'done

> (get-register-contents recursive-exponentiation-machine 'val)
4
}

@section[#:tag "c5e17"]{Exercise 5.17}

Printing labels alongside instructions is slightly trickier because we aren't
currently associating instructions with their preceding labels.

We could use an auxiliary data structure to store the associations between
instructions and labels (where applicable), but it seems a little cleaner
to me to expand the instruction type to optionally include a label. Since
the underlying data structure is hidden behind accessing functions, the change
should be relatively painless.

@tt{extract-labels} is procedure which iterates over the labels and instructions
in the program and separates them. One might assume that this is the best
location to associate instructions with their label(s), but this happens to
iterate over the instructions in reverse order. However, the iteration occurs in
reverse order. If labels are discovered after the instruction at which they
point, we need to be slightly more careful in how we track this.

I would still like to maintain the association between labels and instructions
by instrumenting @tt{extract-labels}, because this is the function that is
canonically responsible for constructing both of these. The main change
that I need to make now is to maintain a reference to the @italic{last}
instruction which I have created and add labels to it where necessary. In
essence, I can do this by adding a new argument to the internal function that
this uses.

First, here are the updated constructor/accessor functions for instructions:

@racketblock[
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
]

As you can see, the new representation is to include a list of labels as the
second item in a list, with the instruction text being first and the execution
procedure being last. This choice is arbitrary. (I've not added the ability
to clear the labels that point to an instruction to the public interface.)

Next, here is the new definition of @tt{extract-labels}. Note the new
argument to the internal procedure, and the fact that we only associate an
instruction with a new label if we have one.

@racketblock[
(define (extract-labels text receive)
  (if (null? text)
      (receive '() '() #f)
      (extract-labels (cdr text)
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
]

@tt{assemble}, the only caller of this method, also needs to be modified:

@racketblock[
(define (assemble controller-text machine)
  (extract-labels controller-text
    (lambda (insts labels last-instruction)
      (update-insts! insts labels machine)
      insts)))
]

Lastly, I can update @tt{display-instruction} to also print the labels. For
legibility, I will unconditionally indent real instruction text to keep it
distinct from labels:

@racketblock[
(define (display-instruction inst)
  (if (instruction-labels inst)
      (for-each displayln (instruction-labels inst)))
  (displayln "  " (car inst)))
]

To see these changes in action, here's a modified version of the recursive
exponentiation machine which contains two labels above one of the instructions:

@racketblock[
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
]

And in use:

@verbatim{
> (set-register-contents! recursive-exponentiation-machine 'b 2)
'done

> (set-register-contents! recursive-exponentiation-machine 'n 2)
'done

> (trace-on recursive-exponentiation-machine)

> (start recursive-exponentiation-machine)
start-machine
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
expt-loop
other-label-expt-loop
   (test (op =) (reg n) (const 0))
   (branch (label base-case))
   (save continue)
   (assign n (op -) (reg n) (const 1))
   (save n)
   (assign continue (label after-expt))
   (goto (label expt-loop))
expt-loop
other-label-expt-loop
   (test (op =) (reg n) (const 0))
   (branch (label base-case))
base-case
   (assign val (const 1))
   (goto (reg continue))
after-expt
   (restore n)
   (restore continue)
   (assign val (op *) (reg b) (reg val))
   (goto (reg continue))
after-expt
   (restore n)
   (restore continue)
   (assign val (op *) (reg b) (reg val))
   (goto (reg continue))
expt-done
   (perform (op print-instruction-count))
instruction-count = 28
'done

> (get-register-contents recursive-exponentiation-machine 'val)
4
}

As you can see, the result and the instruction counts are both identical, and
the instructions have been traced correctly.

@section[#:tag "c5e18"]{Exercise 5.18}

Adding register tracing is about as easy as instruction tracing. However, we can
no longer isolate the changes related to tracing solely to the machine itself --
due to the current implementation strategy, @tt{make-assign} ought to be
responsible for the actual printing. I could modify @tt{set-contents!}
itself, but I believe that giving the registers themselves knowledge of whether
tracing should be enable may be unwise. For one, it seems plausible
to me that we ought to be able to treat @tt{set-register-contents!} differently,
because it's a user-facing command that isn't a part of machine execution.
It's also more inconsistent with how we implemented instruction tracing.
This is a judgment call that I don't feel entirely happy about either way.

First, we modify @tt{make-new-machine} to add the new internal state and some
methods for accessing it:

@racketblock[
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
    ...

    (define (dispatch message)
      (cond ...
        ((eq? message 'trace-register-on)
         (lambda (register-name)
          (set! traced-registers (add-unique-value traced-registers register-name))))
        ((eq? message 'trace-register-off)
         (lambda (register-name)
          (set! traced-registers (remove-unique-value traced-registers register-name))))
        ((eq? message 'clear-traced-registers)
         (lambda () (set! traced-registers '())))
        ((eq? message 'tracing-register?)
         (lambda (register-name) (contains traced-registers register-name))))

        ...)))
]

Next, we can add a few top-level helper functions to keep code clean:

@racketblock[
(define (trace-register-on machine register-name)
  ((machine 'trace-register-on) register-name))
(define (trace-register-off machine register-name)
  ((machine 'trace-register-off) register-name))
(define (clear-traced-registers machine)
  ((machine 'clear-traced-registers)))
(define (tracing-register? machine register-name)
  ((machine 'tracing-register?) register-name))
]

Next, we modify @tt{make-assign} so that it prints out the assignment
it will perform just before it does so:

@racketblock[
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
]

Lastly, we make a similar modification to @tt{restore}:

@racketblock[
(define (make-restore inst machine stack pc)
  (let* ((register-name (stack-inst-reg-name inst))
         (reg (get-register machine register-name)))
    (lambda ()
      (let ((value (pop stack)))
        (if (tracing-register? machine register-name)
            (displayln "Restoring" register-name "from" (get-contents reg) "to" value))
        (set-contents! reg value)
        (advance-pc pc)))))
]

I'm going to opt not to modify @tt{set-register-contents!} for the sake of brevity,
but it would be even easier to modify.

Here it is in action:

@verbatim{
> (set-register-contents! recursive-exponentiation-machine 'b 2)
'done

> (set-register-contents! recursive-exponentiation-machine 'n 2)
'done

> (trace-register-on recursive-exponentiation-machine 'n)

> (start recursive-exponentiation-machine)
Assigning n from 2 to 1
Assigning n from 1 to 0
Restoring n from 0 to 0
Restoring n from 0 to 1
instruction-count = 28
'done
}

@section[#:tag "c5e19"]{Exercise 5.19}

Implementing breakpoints based on labels is an interesting change. Every label
contains a reference to the instruction it points to. We can use the @tt{eq?}
function to check whether two objects are identical (which will suffice to
distinguish two different instances of the "same" instruction). In this way,
our implementation strategy is straightforward. First, we initialize the machine
with a list of active breakpoints, beginning empty. Next, we make sure to
save the labels in the machine after assembly so we can look them up later.

After this preparatory work, we can add methods for manipulating the list
of active breakpoints. To add a breakpoint, we look up a label by name, find
the @italic{n}th instruction after it (or raise an error if it doesn't exist),
and add it to the set of active breakpoints. Then, when we are executing instructions
we check whether an @italic{identical} instruction exists in the list of active
breakpoints, halting the machine if this is true. When we're done, we can tell the
machine to continue running.

This also requires a change to the way machine execution works. Right now, there's no
opportunity for the machine to stop executing instructions. One solution is to save
a continuation to continue execution where it left off when we're ready to do so.
Scheme has continuation features built into the language, but for the sake of clarity
we'll implement this ourselves.

Let's go through an MVP implementation of breakpoints for the machine. @bold{An
exercise for later would be to expand this implementation to be more ergonomic.}

First, we define a few useful helper functions for working with lists of unique
values by identity:

@racketblock[
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
]

Note that these functions are almost identical to their non-identity versions.
This means that it would be feasible to extract @tt{equal?}/@tt{eq?} from them
and pass the comparison function in as a parameter. An example of what that
would look like is below:

@racketblock[
(define (add-unique-value-by lst value compare)
  (define (loop rest)
    (cond ((null? rest) (cons value lst))
          ((compare (car rest) value) lst)
          (else (loop (cdr rest)))))
  (loop lst))

(define (add-unique-value lst value)
  (add-unique-value-by lst value equal?))

(define (add-unique-value-id lst value)
  (add-unique-value-by lst value eq?))
]

(Currying would also be an option.)

Next, we have to track additional state inside the machine:

@racketblock[
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
    ...))
]

Inside this @tt{let} body, we need to define a few new internal functions
for adding and removing breakpoints:

@racketblock[
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
]

Next, the meat of the work: Reworking @tt{execute} to handle continuations
and save them if it reaches a breakpoint:

@racketblock[
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
]

I've chosen to attach this directly to @tt{execute} to make sure that this
continuation behavior is global. I could imagine moving the execution of
the continuation into the function for resuming execution of a paused machine
(shown below), but I believe this would be rather likely to lead to situations
where breakpoints are not handled correctly.

There are a number of new messages that our machine needs to handle. Also note
that @tt{start} needs to clear any continuation before beginning execution.

@racketblock[
(define (dispatch message)
  (cond ((eq? message 'start)
         (set-contents! pc the-instruction-sequence)
         (set! continuation #f)
         (execute))
        ...
        ((eq? message 'install-labels)
         (lambda (seq) (set! labels seq)))
        ((eq? message 'add-breakpoint)
         (lambda (label n) (add-breakpoint (nth-instruction-after label n))))
        ((eq? message 'remove-breakpoint)
         (lambda (label n) (remove-breakpoint (nth-instruction-after label n))))
        ((eq? message 'remove-all-breakpoints)
         (lambda () (set! active-breakpoints '())))
        ((eq? message 'resume-execution)
         (lambda () (execute)))
         ...))
]

Of course, we define the top-level interface for working with breakpoints on a
machine:

@racketblock[
(define (set-breakpoint machine label n)
  ((machine 'add-breakpoint) label n))
(define (clear-breakpoint machine label n)
  ((machine 'remove-breakpoint) label n))
(define (cancel-all-breakpoints machine)
  ((machine 'remove-all-breakpoints)))
(define (proceed-machine machine)
  ((machine 'resume-execution)))
]

The last piece that remains is getting the labels into the machine.
This means that @tt{make-machine} needs to be expanded to save the
labels as well as the instructions:

@racketblock[
(define (make-machine ops controller-text)
  (let ((machine (make-new-machine)))
    ((machine 'install-operations) ops)
    (let* ((instructions-and-labels (assemble controller-text machine))
           (instructions (car instructions-and-labels))
           (labels (cdr instructions-and-labels)))
      ((machine 'install-instruction-sequence) instructions)
      ((machine 'install-labels) labels)
      machine)))
]

@tt{assemble} also needs to return these labels:

@racketblock[
(define (assemble controller-text machine)
  (extract-labels controller-text
    (lambda (insts labels last-instruction)
      (update-insts! insts labels machine)
      (cons insts labels))))
]

With this, the implementation is essentially done. As a final note, you may have
noticed that I've added a few accessor methods for the different pieces of
a label. These are trivial:

@racketblock[
(define (label-entry-name label)
  (car label))
(define (label-entry-instructions label)
  (cdr label))
]

With this, our machine now supports breakpoints:

@verbatim{
> (set-breakpoint recursive-exponentiation 'after-expt 2)

> (set-breakpoint recursive-exponentiation-machine 'start-machine 1)

> (set-register-contents! recursive-exponentiation-machine 'val 0)
'done

> (set-register-contents! recursive-exponentiation-machine 'n 2)
'done

> (set-register-contents! recursive-exponentiation-machine 'b 2)
'done

> (start recursive-exponentiation-machine)
Paused on breakpoint

> (get-register-contents recursive-exponentiation-machine 'n)
2

> (get-register-contents recursive-exponentiation-machine 'b)
2

> (get-register-contents recursive-exponentiation-machine 'val)
0

> (proceed-machine recursive-exponentiation-machine)
Resuming execution
Paused on breakpoint

> (get-register-contents recursive-exponentiation-machine 'b)
2

> (get-register-contents recursive-exponentiation-machine 'n)
0

> (get-register-contents recursive-exponentiation-machine 'val)
1

> (proceed-machine recursive-exponentiation-machine)
Resuming execution
Paused on breakpoint

> (get-register-contents recursive-exponentiation-machine 'val)
2

> (get-register-contents recursive-exponentiation-machine 'n)
1

> (get-register-contents recursive-exponentiation-machine 'b)
2

> (proceed-machine recursive-exponentiation-machine)
Resuming execution
instruction-count = 28
'done

> (get-register-contents recursive-exponentiation-machine 'val)
4

> (cancel-all-breakpoints recursive-exponentiation-machine)

> (set-register-contents! recursive-exponentiation-machine 'b 2)
'done

 (set-register-contents! recursive-exponentiation-machine 'n 2)
'done

> (set-register-contents! recursive-exponentiation-machine 'val 0)
'done

> (start recursive-exponentiation-machine)
instruction-count = 28
'done

> (get-register-contents recursive-exponentiation-machine 'val)
4
}

This implementation basically works, but it has a few problems in practice. Most notably,
no information about what the breakpoint was about is maintained, so there's no
way to print out something all that reasonable when execution is paused (and no,
printing out the instruction text is @italic{not} reasonable by itself). Solving
this problem in a good way would probably require a lot more instrumentation of
the instruction list, so you could display the instruction after the breakpoint
and its surrounding context (as well as some globally-useful information, such as
the index of the instruction in the total program, or the most recent label before it).
Instruction tracing would help slightly with this problem, but at the expense of
printing a lot more output than you may want.

The limitations of the interface for working with the machine are also slightly
more obvious now. A modern debugger is capable of displaying much more of the current
program state at once -- local variables, stack traces, &etc. We only have functions
for getting individual register values. The machine doesn't even expose its instruction
sequence to the outside world.

All of these are important things to do in the real world. However, I've chosen to
skip all of them to keep the changes for this exercise concentrated on the essential
changes needed to support breakpoints.

@section[#:tag "c5e20"]{Exercise 5.20}

We are asked to draw box-and-pointer and memory representations of the following list:

@racketblock[
(define x (cons 1 2))
(define y (list x x))
]

Additionally, we are told that the @tt{free} pointer is initially at @tt{p1}.

If we perform these allocations in order, then we will first put a @tt{cons}
cell with the number 1 in both the @tt{car} and @tt{cdr} slots in slot @tt{p1}.
I believe this is uncontroversial -- we may not have a list, but @tt{car} and
@tt{cdr} of any kind of pair are straightforward.

Next, we should add a @tt{cons} cell with the @tt{car} pointing at @tt{p0} and the
@tt{cdr} slot pointing to the empty list @tt{e0}. I believe that the most reasonable
way to fill out memory when constructing lists with the @tt{list} operator is to
construct them in reverse order, with the last @tt{cons} cell being allocated first.
This is easier to implement because it allows the earlier cells to refer to cells
that have already been created. This will also let us take advantage of the normal
incrementing of the @tt{free} pointer.

It follows that we place the next (and first) @tt{cons} cell of @tt{y} in
slot @tt{p3}. This points to @tt{p0} in the @tt{car} position and @tt{p2} in
the @tt{cdr} position.

After we're done, the @tt{free} pointer is at position @tt{p4}.

Diagrams follow:

@image["img/5-20_box-pointer.png"]

@image["img/5-20_memory.png"]

@section[#:tag "c5e21"]{Exercise 5.21}

Before I begin: I believe that the sentence "Assume that the list-structure memory
operations are available as machine primitives." is telling me that operations like
@tt{null?}, @tt{cons}, &etc are already provided, @italic{not} just @tt{vector-ref}
&etc.

@tt{count-leaves} is a little more complex than the other programs we've written
so far because it has two recursive calls. We've seen an example of this before,
but we've never written an example ourselves.

First, we have a recursive implementation. This makes two recursive calls
in sequence and adds the results. For this to work, we need to save the return
value of the first call in order to add it to the return value of the second
call. The only other thing I'd like to point out is that we only @tt{save} the
@tt{continue} register before making the first recursive call. There's no need
to do this after the second, because the destination would be the same.

@racketblock[
(define (count-leaves tree)
  (cond ((null? tree) 0)
        ((not (pair? tree)) 1)
        (else (+ (count-leaves (car tree))
                 (count-leaves (cdr tree))))))
]

@racketblock[
start-machine
  (assign continue (label count-leaves-done))
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
count-leaves-done
]

The instrumentation we did in the previous section makes it easy to watch how
this machine behaves:

@verbatim{
> (set-register-contents! recursive-count-leaves-machine 'tree '(1 (2 (3 4 (5) 6) 7)))
'done

> (trace-register-on recursive-count-leaves-machine 'val)

> (trace-register-on recursive-count-leaves-machine 'tree)

> (start recursive-count-leaves-machine)
Assigning val from *unassigned* to 0
Assigning tree from (1 (2 (3 4 (5) 6) 7)) to 1
Assigning val from 0 to 1
Restoring tree from 1 to (1 (2 (3 4 (5) 6) 7))
Assigning tree from (1 (2 (3 4 (5) 6) 7)) to ((2 (3 4 (5) 6) 7))
Assigning tree from ((2 (3 4 (5) 6) 7)) to (2 (3 4 (5) 6) 7)
Assigning tree from (2 (3 4 (5) 6) 7) to 2
Assigning val from 1 to 1
Restoring tree from 2 to (2 (3 4 (5) 6) 7)
Assigning tree from (2 (3 4 (5) 6) 7) to ((3 4 (5) 6) 7)
Assigning tree from ((3 4 (5) 6) 7) to (3 4 (5) 6)
Assigning tree from (3 4 (5) 6) to 3
Assigning val from 1 to 1
Restoring tree from 3 to (3 4 (5) 6)
Assigning tree from (3 4 (5) 6) to (4 (5) 6)
Assigning tree from (4 (5) 6) to 4
Assigning val from 1 to 1
Restoring tree from 4 to (4 (5) 6)
Assigning tree from (4 (5) 6) to ((5) 6)
Assigning tree from ((5) 6) to (5)
Assigning tree from (5) to 5
Assigning val from 1 to 1
Restoring tree from 5 to (5)
Assigning tree from (5) to ()
Assigning val from 1 to 0
Assigning val from 0 to 1
Restoring tree from () to ((5) 6)
Assigning tree from ((5) 6) to (6)
Assigning tree from (6) to 6
Assigning val from 1 to 1
Restoring tree from 6 to (6)
Assigning tree from (6) to ()
Assigning val from 1 to 0
Assigning val from 0 to 1
Assigning val from 1 to 2
Assigning val from 2 to 3
Assigning val from 3 to 4
Restoring tree from () to ((3 4 (5) 6) 7)
Assigning tree from ((3 4 (5) 6) 7) to (7)
Assigning tree from (7) to 7
Assigning val from 4 to 1
Restoring tree from 7 to (7)
Assigning tree from (7) to ()
Assigning val from 1 to 0
Assigning val from 0 to 1
Assigning val from 1 to 5
Assigning val from 5 to 6
Restoring tree from () to ((2 (3 4 (5) 6) 7))
Assigning tree from ((2 (3 4 (5) 6) 7)) to ()
Assigning val from 6 to 0
Assigning val from 0 to 6
Assigning val from 6 to 7
'done
}

An iterative implementation is similar, but features a very prominent difference:
because the algorithm is tail-recursive, we don't need to perform any operations
after the second recursive call. The final return value should already be in the
@tt{val} register. Much of the rest of the program is similar.

@racketblock[
(define (count-leaves tree)
  (define (count-iter tree n)
    (cond ((null? tree) n)
          ((not (pair? tree)) (+ n 1))
          (else (count-iter (cdr tree)
                            (count-iter (car tree) n)))))
  (count-iter tree 0))
]

@racketblock[
start-machine
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
count-leaves-done
]

Similarly, we can instrument this program and observe how it behaves:

@verbatim{
> (set-register-contents! iterative-count-leaves-machine 'tree '(1 (2 (3 4 (5) 6) 7)))
'done

> (trace-register-on iterative-count-leaves-machine 'val)

> (trace-register-on iterative-count-leaves-machine 'tree)

> (trace-register-on iterative-count-leaves-machine 'n)

> (start iterative-count-leaves-machine)
Assigning val from *unassigned* to 0
Assigning n from *unassigned* to 0
Assigning tree from (1 (2 (3 4 (5) 6) 7)) to 1
Assigning val from 0 to 1
Restoring tree from 1 to (1 (2 (3 4 (5) 6) 7))
Assigning tree from (1 (2 (3 4 (5) 6) 7)) to ((2 (3 4 (5) 6) 7))
Assigning n from 0 to 1
Assigning tree from ((2 (3 4 (5) 6) 7)) to (2 (3 4 (5) 6) 7)
Assigning tree from (2 (3 4 (5) 6) 7) to 2
Assigning val from 1 to 2
Restoring tree from 2 to (2 (3 4 (5) 6) 7)
Assigning tree from (2 (3 4 (5) 6) 7) to ((3 4 (5) 6) 7)
Assigning n from 1 to 2
Assigning tree from ((3 4 (5) 6) 7) to (3 4 (5) 6)
Assigning tree from (3 4 (5) 6) to 3
Assigning val from 2 to 3
Restoring tree from 3 to (3 4 (5) 6)
Assigning tree from (3 4 (5) 6) to (4 (5) 6)
Assigning n from 2 to 3
Assigning tree from (4 (5) 6) to 4
Assigning val from 3 to 4
Restoring tree from 4 to (4 (5) 6)
Assigning tree from (4 (5) 6) to ((5) 6)
Assigning n from 3 to 4
Assigning tree from ((5) 6) to (5)
Assigning tree from (5) to 5
Assigning val from 4 to 5
Restoring tree from 5 to (5)
Assigning tree from (5) to ()
Assigning n from 4 to 5
Assigning val from 5 to 5
Restoring tree from () to ((5) 6)
Assigning tree from ((5) 6) to (6)
Assigning n from 5 to 5
Assigning tree from (6) to 6
Assigning val from 5 to 6
Restoring tree from 6 to (6)
Assigning tree from (6) to ()
Assigning n from 5 to 6
Assigning val from 6 to 6
Restoring tree from () to ((3 4 (5) 6) 7)
Assigning tree from ((3 4 (5) 6) 7) to (7)
Assigning n from 6 to 6
Assigning tree from (7) to 7
Assigning val from 6 to 7
Restoring tree from 7 to (7)
Assigning tree from (7) to ()
Assigning n from 6 to 7
Assigning val from 7 to 7
Restoring tree from () to ((2 (3 4 (5) 6) 7))
Assigning tree from ((2 (3 4 (5) 6) 7)) to ()
Assigning n from 7 to 7
Assigning val from 7 to 7
'done
}

@section[#:tag "c5e22"]{Exercise 5.22}

We are asked to reimplement @tt{append} and @tt{append!} in our assembly language.

@racketblock[
(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))
]

@racketblock[
start-machine
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
append-done
]

@racketblock[
(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))
]

If we ignore the function separation between @tt{append!} and @tt{last-pair},
we can write a very compact procedure:

@racketblock[
last-pair-init
  (assign x-iter (reg x))
last-pair-recurse
  (assign temp (op cdr) (reg x-iter))
  (test (op null?) (reg temp))
  (branch (label perform-append))
  (assign x-iter (reg temp))
  (goto (label last-pair-recurse))
perform-append
  (perform (op set-cdr!) (reg x-iter) (reg y))
]

However, we ought to go the extra mile and make @tt{last-pair} a real function.
By my implicit calling conventions, this means that it should return a value
in the @tt{val} register and should not require a special communication register
like @tt{x-iter}. This version is quite similar: The only substantive differences
are that we @tt{save} the value of @tt{x} before starting to recurse, and that
we assign the last @tt{x} value we encounter to the @tt{val} register before
exiting.

@racketblock[
last-pair-init
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
  (perform (op set-cdr!) (reg val) (reg y))
]

Both of these machines work, but it should be noted that @tt{append!} doesn't
support appending to an empty list, due to its use of @tt{set-cdr!}.

@verbatim{
> (set-register-contents! recursive-append-machine 'x '(1 2 3))
'done

> (set-register-contents! recursive-append-machine 'y '(4 5 6))
'done

> (start recursive-append-machine)
'done

> (get-register-contents recursive-append-machine 'val)
(mcons 1 (mcons 2 (mcons 3 (mcons 4 (mcons 5 (mcons 6 '()))))))

> (set-register-contents! iterative-append-machine 'x '(1 2 3))
'done

> (set-register-contents! iterative-append-machine 'y '(4 5 6))
'done

> (start iterative-append-machine)
'done

> (get-register-contents iterative-append-machine 'x)
(mcons 1 (mcons 2 (mcons 3 (mcons 4 (mcons 5 (mcons 6 '()))))))
}

@section[#:tag "c5e23"]{Exercise 5.23}

Back in chapter 4, we defined a few types of expressions as transformations
of other, more "basic" expressions that we already supported. For example,
@tt{cond} expressions can be transformed into @tt{if}s, @tt{let}s can be
defined in terms of immediately-invoked @tt{lambda}s, and so on. Since the book
does not detail exactly what forms we'll support, I'll implement @tt{cond} as well
as all three types of @tt{let} constructs supported by Scheme, but not the
"extra" constructs like @tt{while}.

Since we're implementing them while assuming that transformation functions
such as @tt{cond->if} are available as operations, our methodology can be
very straightforward: We call the operation to transform the value contained
in the @tt{exp} register, save it back into @tt{exp}, and then re-evaluate
the expression by branching to @tt{eval-dispatch} again. This is very similar
to how we called @tt{eval} recursively on the transformed expressions earlier.

The most straightforward way to do this is to duplicate the re-evaluation code
for each of these cases. It would be possible to implement a single sub-procedure
which receives an operation as an argument and jump to it from different locations.
However elegant this idea may seem, I think it would require significantly more
code in addition to being more complex, so I'm not going to do this.

Assuming we've brought over the required operations and their dependencies
(which will not be repeated here), the changes to the interpreter are very
easy. First, we expand @tt{eval-dispatch} to check for our new expression types
@italic{before} trying to perform function application:

@racketblock[
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
]

Next, we add the new evaluation methods. They are all almost exactly
the same:

@racketblock[
ev-cond
 (assign exp (op cond->if) (reg exp))
 (goto (label eval-dispatch))

ev-let
 (assign exp (op let->combination) (reg exp))
 (goto (label eval-dispatch))

ev-let*
 (assign exp (op let*->nested-lets) (reg exp))
 (goto (label eval-dispatch))
]

And that's all. There isn't much to verify, but we can give it a spot check:

@verbatim{
;;; EC-EVAL input:
(let ((x 1)) (+ x 1))

;;; EC-Eval value:
2

;;; EC-EVAL input:
(let* ((x 1) (y (+ x 1))) (* y 2))

;;; EC-Eval value:
4

;;; EC-EVAL input:
(define (fib n)
  (let fib-iter ((a 1)
                 (b 0)
                 (count n))
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1)))))

;;; EC-Eval value:
ok

;;; EC-EVAL input:
(fib 10)

;;; EC-Eval value:
55

;;; EC-EVAL input:
(cond (else 1))

;;; EC-Eval value:
1

;;; EC-EVAL input:
(cond ((> 1 2) 1) (else 2))

;;; EC-Eval value:
2
}

@section[#:tag "c5e24"]{Exercise 5.24}

Interpreting @tt{cond} expressions as first-order constructs, rather than
simplifying them into @tt{if} expressions, requires slightly more work. The
most important piece of it is to keep track of which branches have yet to be
evaluated. As usual, we can use the @tt{unev} register to maintain a reference
to a list of @tt{cond} branches that we haven't checked yet.

We can use @tt{cond-clauses} as a machine primitive to get a reference to the clauses.
Of course, we'll also have to allow @tt{else} expressions, and verify that they only
appear as the final alternative in the expression. I've chosen not to implement
support for the alternate form at this time, largely because I never use it.

First, we need some new procedures for extracting parts of @tt{cond} expressions:

@racketblock[
(define (cond-first-predicate exp)
  (cond-predicate (first-exp exp)))
(define (cond-first-actions exp)
  (cond-actions (first-exp exp)))
(define (cond-else-predicate? exp)
  (eq? exp 'else))
]

Naturally, these will need to be added to the list of operations supported
by the machine. I've also added @tt{null?}, which we've managed to get away
with not having until now.

For simplicity's sake, we can implement the @tt{cond} expression without
tail recursion on the alternatives for now. That assembly looks like the
following:

@racketblock[
ev-begin-cond
 (assign unev (op cond-clauses) (reg exp))
 (save continue)

ev-cond
 (test (op null?) (reg unev))
 (branch (label ev-cond-finished))
 (assign exp (op cond-first-predicate) (reg unev))
 (test (op cond-else-predicate?) (reg exp))
 (branch (label ev-cond-else-test))
 (save env)
 (save unev)
 (assign continue (label ev-cond-test-predicate))
 (goto (label eval-dispatch))

ev-cond-test-predicate
 (restore unev)
 (restore env)
 (test (op true?) (reg val))
 (branch (label ev-cond-body))
 (assign unev (op rest-exps) (reg unev))
 (goto (label ev-cond))

ev-cond-body
 (assign unev (op cond-first-actions) (reg unev))
 (assign continue (label ev-cond-finished))
 (goto (label ev-sequence))

ev-cond-else-test
 (test (op last-exp?) (reg unev))
 (branch (label ev-cond-else))
 (goto (label invalid-cond-else-form))

ev-cond-else
 (assign unev (op cond-first-actions) (reg unev))
 (assign continue (label ev-cond-finished))
 (goto (label ev-sequence))

ev-cond-finished
 (restore continue)
 (goto (reg continue))

invalid-cond-else-form
 (assign val (const invalid-cond-else-form-error))
 (goto (label signal-error))
]
