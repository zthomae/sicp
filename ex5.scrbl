#lang scribble/manual

@require[scribble/lp]
@require[scribble/examples]
@require["eval.rkt"]

@title[#:version "" #:style 'toc]{Chapter 5}

@define[ev @make-eval[]]

@section[#:tag "c5e1"]{Exercise 5.1}

This is a literal translation of the iterative factorial
algorithm. The product and counter are both initialized to
@tt{1} at the start, mimicking the initial call to the inner
recursive function. Then, in every iteration, we check
whether the counter is greater than the original input
@tt{n}. If it is, we finish, with the final result being
stored in the @tt{product} register. If it isn't, then we
update the product and increment the counter before starting
again.

@image["img/factorial-register-machine.png"]

@section[#:tag "c5e2"]{Exercise 5.2}

A definition of the factorial machine using the provided
assembly language would look something like this:

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

@image["img/newton-method-1.png"]

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

Next, here is an expanded version with those functions expanded. (Note: the
diagram has separated some of the re-assignments into separate registers
purely for readability).

@image["img/newton-method-2.png"]

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
