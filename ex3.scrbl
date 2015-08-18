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

There is one subtlety in this code: Because the inner @tt{dispatch}
procedure is always expected to return a procedure, we have to
wrap the sending of the "Incorrect password" message in a dummy
procedure. I have done this in a rather terse way.

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
        (lambda args "Incorrect password")))
  dispatch)
}

@section[#:tag "c3e4"]{Exercise 3.4}

I also shouldn't need to mention that this is not a robust way
to deal with possible attempted account theft.

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
   (define (dispatch p m)
    (if (eq? p password)
        (cond ((eq? m 'withdraw) withdraw)
                 ((eq? m 'deposit) deposit)
                 (else (error "Unknown request -- MAKE-ACCOUNT" m)))
        (begin
          (set! incorrect-count (+ incorrect-count 1))
          (if (> incorrect-count 7) (call-the-cops))
          (lambda args "Incorrect password"))))
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

Making a joint account is not difficult: All you need to do is add
a new layer of password checking (so that the new password only
works with the newly-created joint account). This behaves similarly
to the password checking we already made, except it returns the unlocked
main account if the given joint password is correct.

@codeblock{
(define (make-joint account password new-password)
  (lambda (p m)
    (if (eq? p new-password)
        (account password m)
        (lambda args "Incorrect password"))))
}

@section[#:tag "c3e8"]{Exercise 3.8}

I feel like I've come up with an ugly solution. I'm going to mark this as
@bold{TODO} for now.

@codeblock{
(define f
  (let ((y 0))
    (lambda (x)
      (if (= y 0)
          (begin
            (set! y x)
            0)
          (begin
            (set! y x)
            1)))))
}

I verified that this worked by swapping the order of the calls. Assuming
the order of evaluation is fixed, this should demonstrate the different
behaviors that would happen if the order of evaluation was actually changed.

@section[#:tag "c3e9"]{Exercise 3.9}

@bold{TODO: ASCII art}

@section[#:tag "c3e10"]{Exercise 3.10}

The new @tt{make-withdraw} is subtly different from the earlier examples
in that the body is not merely a @tt{lambda} expression, but an immediately-
invoked @tt{lambda}.

(This is somewhat difficult to explain without diagrams, but I've put the
skeleton of my answer here anyway. @bold{It's probably wrong.})

The first thing we should do before trying to apply the environment
model is to fully expand on all syntactic sugar around @tt{lambda}
expressions. When we do this, the definition for @tt{make-withdraw} is
as follows:

@codeblock{
(define make-withdraw
  (lambda (initial-amount)
    ((lambda (balance)
       (lambda (amount)
         (if (>= balance amount)
             (begin
               (set! balance (- balance amount))
               balance)
             "Insufficient funds")))
     initial-amount)))
}

Here, @tt{make-withdraw} will be a name in the global environment
referring to the procedure inside.

When we evaluate the expression @tt{(define W1 (make-withdraw 100))},
we create a name @tt{W1} in the global environment that refers to the
result of @tt{(make-withdraw 100)}. When this is evaluated, a new
environment, which we'll call @tt{E1}, is constructed with the global
environment as its parent. It has @tt{initial-amount} bound to @tt{100}.
The procedure we are evaluating in this environment has the body

@codeblock{
((lambda (balance)
   (lambda (amount)
     (if (>= balance amount)
         (begin
           (set! balance (- balance amount))
           balance)
         "Insufficient funds")))
 initial-amount)
}

This body is a procedure application, so it creates a new environment, @tt{E2},
whose parent is the environment in which this procedure was defined --
that is, @tt{E1}. However, the procedure which is being applied is also
defined here. So we create a procedure object that takes one parameter, whose
defining environment is @tt{E2}, and with the body

@codeblock{
(lambda (amount)
  (if (>= balance amount)
      (begin
        (set! balance (- balance amount))
        balance)
      "Insufficient funds"))
}

This procedure object is returned and given the name @tt{W1}.

Then we evaluate @tt{(W1 50)}. This creates a new environment, @tt{E3}, whose
parent frame is @tt{E2}, the environment in which the procedure that has
the name @tt{W1} was defined. We then evaluate the procedure, which updates
the @tt{balance} value stored in @tt{E2} from @tt{100}, the value initially
given from @tt{initial-amount} in @tt{E1}, to @tt{50}.

Evaluating @tt{(define W2 (make-withdraw 100))} makes a parallel procedure and
set of environments like @tt{W1}, with its own private @tt{balance}.

@bold{TODO: Pretty pictures}

@section[#:tag "c3e11"]{Exercise 3.11}

