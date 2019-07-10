#lang scribble/manual

@require[scribble/lp]
@require[scribble/examples]
@require["eval.rkt"]

@title[#:version "" #:style 'toc]{Chapter 5}

@define[ev @make-eval[]]

@section[#:tag "c5e1"]{Exercise 5.1 - 2}

This is a literal translation of the iterative factorial
algorithm. The product and counter are both initialized to
@tt{1} at the start, mimicking the initial call to the inner
recursive function. Then, in every iteration, we check
whether the counter is greater than the original input
@tt{n}. If it is, we finish, with the final result being
stored in the @tt{product} register. If it isn't, then we
update the product and increment the counter before starting
again.

I don't think creating diagrams for the data paths is a good
use of my time (they're hard to work with and confuse me
more than help me -- sorry!). That said, here's an implementation of
the given factorial function:

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
  (branch after-expt)
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
