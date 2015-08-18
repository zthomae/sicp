#lang scribble/manual

@require[scribble/lp]

@title[#:version "" #:style 'toc]{Chapter 3}

@section[#:tag "c3e1"]{Exercise 3.1}

Making an accumulator is essentially the same thing as what was done
in @tt{make-withdraw} -- we define it to be a function that uses
@tt{set!} to modify the argument passed into the constructor. The code
is self-explanatory.

@codeblock{
(define (make-accumulator initial)
  (lambda (more)
    (begin (set! initial (+ initial more))
           initial)))
}

@section[#:tag "c3e2"]{Exercise 3.2}

In this exercise, we create a procedure that decorates a given procedure
and maintains an inner accumulator (possibly created using @tt{make-accumulator}
but in this case not) which it uses to count how many times the decorated
procedure has been called. Additionally, you can pass the argument of
@tt{'how-many-calls?} to get the current value of the counter, and
@tt{reset-count} to set the counter to @tt{0}.

Although the given example of decorating @tt{sqrt} doesn't suggest it,
it's reasonable to expect that we should allow for decorating procedures
of any number of arguments, rather than just one. This implementation handles
messages in the simplest way in that it only checks if the first argument
is equal to a special message. If so, the other arguments are ignored.

@codeblock{
(define (make-monitored f)
  (let ((count 0))
    (lambda args
      (cond ((eq? 'how-many-calls? (car args))
             count)
            ((eq? 'reset-count (car args))
             (set! count 0))
            (else (begin
                    (set! count (+ count 1))
                    (apply f args)))))))
}

@section[#:tag "c3e3"]{Exercise 3.3}

I shouldn't need to mention that handling plaintext passwords is a bad idea.

@codeblock{
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (if (>= amount 0)
        (begin (set! balance (+ balance amount))
               balance)
        "Must deposit a non-negative amount"))
  (define (dispatch p m)
    (if (eq? p password)
        (cond ((eq? m 'withdraw) withdraw)
                 ((eq? m 'deposit) deposit)
                 (else (error "Unknown request -- MAKE-ACCOUNT" m)))
        "Incorrect password"))
  dispatch)
}

@section[#:tag "c3e4"]{Exercise 3.4}

I also shouldn't need to mention that this is not a robust way
to deal with possible attempted account theft.

There is one subtlety in this code: Because the inner @tt{dispatch}
procedure is always expected to return a procedure, we have to
wrap the sending of the "Incorrect password" message in a dummy
procedure.

@codeblock{
(define (make-account balance password)
  (define incorrect-count 0)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (if (>= amount 0)
        (begin (set! balance (+ balance amount))
               balance)
        "Must deposit a non-negative amount"))
  (define (incorrect-password . args)
    "Incorrect password")
  (define (dispatch p m)
    (if (eq? p password)
        (cond ((eq? m 'withdraw) withdraw)
                 ((eq? m 'deposit) deposit)
                 (else (error "Unknown request -- MAKE-ACCOUNT" m)))
        (begin
          (set! incorrect-count (+ incorrect-count 1))
          (if (> incorrect-count 7) (call-the-cops))
          incorrect-password)))
  dispatch)
}

@section[#:tag "c3e5"]{Exercise 3.5}

In the interest of sharing, I thought I'd show you that I made a @tt{rand}
of my own by using the @tt{random} function (which computes a random number
from @tt{0} to its given argument) with the biggest value it will accept.

@codeblock{
(define (rand)
  (random 4294967087))
}

It's not pretty, but it works, and allows you to verify that the Monte Carlo
methods actually work. Now, @bold{back to regular programming}.

Defining a new Monte Carlo experiment is easy: All we need to do is define
a procedure of no parameters for our experiment and pass it, along with a
given number of trials, to @tt{monte-carlo}. The experiment procedure
can use whatever parameters it wants, as long as it takes no parameters
to run (that is, you can use parameters to set up the kind of experiment).

In this case, we set up the experiment procedure with the bounds of the
rectangle we are generating points in and the predicate we want to test.
@tt{estimate-integral} turns out to be a simple procedure:

@codeblock{
(define (estimate-integral P trials x1 x2 y1 y2)
  (define (test-point)
    (let ((x (random-in-range x1 x2))
          (y (random-in-range y1 y2)))
      (P x y)))
  (monte-carlo trials test-point))
}

We can define the predicate in the book like this:

@codeblock{
(define (P x y)
  (>= 9
      (+ (square (- x 5))
         (square (- y 7)))))
}

@section[#:tag "c3e6"]{Exercise 3.6}

Wrapping @tt{rand} in a message handler that allows for generating new
random numbers as well as resetting the current random number with the
initial value (the @tt{random-init} value) is not especially hard. I've
chosen to have the @tt{'generate} message return the new random number,
while the @tt{'reset} message returns nothing of value.

@codeblock{
(define (rand m)
  (let ((x random-init))
    (cond ((eq? m 'generate)
           (begin
             (set! x (rand-update x))
             x))
          ((eq? m 'reset)
           (set! x random-init))
          (else (error "Invalid message -- RAND" m)))))
}

@section[#:tag "c3e7"]{Exercise 3.7}

