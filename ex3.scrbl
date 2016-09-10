#lang scribble/manual

@require[scribble/lp]
@require[scribble/examples]
@require["eval.rkt"]

@title[#:version "" #:style 'toc]{Chapter 3}

@define[ev @make-eval[]]

@section[#:tag "c3e1"]{Exercise 3.1}

Making an accumulator is essentially the same thing as what was done
in @tt{make-withdraw} -- we define it to be a function that uses
@tt{set!} to modify the argument passed into the constructor. The code
is self-explanatory.

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (make-accumulator initial)
   (lambda (more)
     (begin (set! initial (+ initial more))
            initial)))
 ]

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

@examples[
 #:label #f #:eval ev #:no-prompt
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
 ]

@section[#:tag "c3e3"]{Exercise 3.3}

I shouldn't need to mention that handling plaintext passwords is a bad idea.

There is one subtlety in this code: Because the inner @tt{dispatch}
procedure is always expected to return a procedure, we have to
wrap the sending of the "Incorrect password" message in a dummy
procedure. I have done this in a rather terse way.

@examples[
 #:label #f #:eval ev #:no-prompt
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
 ]

@section[#:tag "c3e4"]{Exercise 3.4}

I also shouldn't need to mention that this is not a robust way
to deal with possible attempted account theft.

@examples[
 #:label #f #:eval ev #:no-prompt
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
 ]

@section[#:tag "c3e5"]{Exercise 3.5}

In the interest of sharing, I thought I'd show you that I made a @tt{rand}
of my own by using the @tt{random} function (which computes a random number
from @tt{0} to its given argument) with the biggest value it will accept.

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (rand)
   (random 4294967087))
 ]

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

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (estimate-integral P trials x1 x2 y1 y2)
   (define (test-point)
     (let ((x (random-in-range x1 x2))
           (y (random-in-range y1 y2)))
       (P x y)))
   (monte-carlo trials test-point))
 ]

We can define the predicate in the book like this:

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (P x y)
   (>= 9
       (+ (square (- x 5))
          (square (- y 7)))))
 ]

@section[#:tag "c3e6"]{Exercise 3.6}

Wrapping @tt{rand} in a message handler that allows for generating new
random numbers as well as resetting the current random number with the
initial value (the @tt{random-init} value) is not especially hard. I've
chosen to have the @tt{'generate} message return the new random number,
while the @tt{'reset} message returns nothing of value.

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (rand m)
   (let ((x random-init))
     (cond ((eq? m 'generate)
            (begin
              (set! x (rand-update x))
              x))
           ((eq? m 'reset)
            (set! x random-init))
           (else (error "Invalid message -- RAND" m)))))
 ]

@section[#:tag "c3e7"]{Exercise 3.7}

Making a joint account is not difficult: All you need to do is add
a new layer of password checking (so that the new password only
works with the newly-created joint account). This behaves similarly
to the password checking we already made, except it returns the unlocked
main account if the given joint password is correct.

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (make-joint account password new-password)
   (lambda (p m)
     (if (eq? p new-password)
         (account password m)
         (lambda args "Incorrect password"))))
 ]

@section[#:tag "c3e8"]{Exercise 3.8}

I feel like I've come up with an ugly solution. I'm going to mark this as
@bold{TODO} for now.

@examples[
 #:label #f #:eval ev #:no-prompt
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
 ]

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

@examples[
 #:label #f #:eval ev #:no-prompt
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
 ]

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

@examples[
 #:label #f #:eval ev #:no-prompt
 (lambda (amount)
   (if (>= balance amount)
       (begin
         (set! balance (- balance amount))
         balance)
       "Insufficient funds"))
 ]

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

@bold{TODO: Even more pictures}

When we evaluate @tt{(define acc (make-account 50))}, we create
a name @tt{acc} in the global environment referring to the result
of the application @tt{(make-account 50)}. This sets up a new
environment subordinate to the global environment with @tt{balance},
the parameter of @tt{make-account}, bound to @tt{50}. Then, we define
procedures @tt{withdraw}, @tt{deposit}, and @tt{dispatch} in this
environment and return the procedure @tt{dispatch}.

Suppose then we evaluate @tt{((acc 'deposit) 40)}. This is a procedure
application, so we create a new environment subordinate to the global
environment. However, the procedure being evaluated is the result of evaluating
@tt{(acc 'deposit)}. So a new environment is created subordinate to the
environment @tt{acc} was defined in with @tt{m}, the formal paramter of
@tt{dispatch}, bound to @tt{'deposit}. The procedure is then evaluated and the
procedure @tt{deposit} from earlier is returned.

This procedure is then called with its formal paramter @tt{amount} bound
to @tt{40}. This occurs in an environment subordinate to the one @tt{withdraw}
was defined in and updates the @tt{balance} set in this environment. The
new balance of @tt{90} is returned.

Similar happens when evaluating @tt{((acc 'withdraw) 60)}.

If we define a new account, as in @tt{(define acc2 (make-account 100))},
it refers to a separate procedure returned as the result of
@tt{(make-account 100)}. @tt{acc} and @tt{acc2} both share entries from
the global environment, as they are both subordinate to it. However, the
actual procedures these names refer to are different and are defined in
different environments, which keeps their local states separate.

@section[#:tag "c3e12"]{Exercise 3.12}

@bold{TODO: Still no diagrams}

Suppose we evaluate @tt{(define x (list 'a 'b))}.

When we evaluate @tt{(cdr x)} the first time, we get the list @tt{'(b ())},
which is the @tt{cdr} that @tt{x} was defined with.

Then suppose we evaluate @tt{(define x (append! x  y))}, where @tt{y} was
defined to be @tt{(list 'c 'd)}. This sets the @tt{nil} pointer that followed
the @tt{'b} in @tt{x} to now point to the list @tt{y}. So now, @tt{(cdr x)}
evaluates to @tt{'(b (c (d ())))}.

@section[#:tag "c3e13"]{Exercise 3.13}

Suppose we define the procedure

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (make-cycle x)
   (set-cdr! (last-pair x) x))
 ]

Then, whe we try to compute @tt{(last-pair x)}, we get an infinite loop,
because the end of @tt{x} now points to the front of @tt{x}. In other words,
we will never come across a pair where the @tt{cdr} is @tt{nil}, so
@tt{last-pair} will never terminate.

@section[#:tag "c3e14"]{Exercise 3.14}

Consider the procedure below:

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (mystery x)
   (define (loop x y)
     (if (null? x)
         y
         (let ((temp (cdr x)))
           (set-cdr! x y)
           (loop temp x))))
   (loop x '()))
 ]

In general, this procedure will reverse a list (while trashing
the list being reversed -- more on this later). Consider this brief
trace:

Suppose @tt{x} is the list @tt{v}, defined as @tt{(list 'a 'b 'c 'd)}.
We start by evaluating @tt{(loop x '())}. Since @tt{x} is not @tt{nil},
we then set aside @tt{(cdr x)}, which is @tt{'(b c d)}, and set the
@tt{cdr} of @tt{x} to @tt{'()}. The list @tt{v} is now @tt{'(a)}. Then
we call @tt{loop} again, as @tt{(loop temp x)}.

Since @tt{temp}, the new @tt{x}, is not null, we set aside its @tt{cdr}
and set the @tt{cdr} to be the list @tt{'(a)}. The list is now @tt{'(b a)}.
We then call @tt{(loop temp x)} again.

We continue looping until @tt{temp} is empty, which will happen after
every element of the list has been prepended to the front of the list
we are now calling @tt{x}. This list is then returned. However, the original
list is still set to @tt{'(a)} -- that is, what @tt{x} was set to in the
first loop. @tt{x} still has the value from the first loop because subsequent
calls to @tt{loop} shadowed it, resulting in the modification of other
lists. The only lists we are left with are this shortened version of @tt{x}
and the fully-reversed original list.

It would possibly be more useful to define a procedure that points @tt{x}
to the reversed list. In that way, we could reverse a list in place, with
the list taking on the name of its reversed self. However, doing so is
fraught with peril. We can't simply make @tt{mystery} return
@tt{(set! x (loop x '()))} because this will only change what the name
@tt{x} points to, not the actual list passed in as a parameter. And
using @tt{set-car!} and @tt{set-cdr!} to change this list is also dangerous.
Suppose we do something like

@codeblock{
(let ((reversed (loop x '())))
  (set-car! x (car reversed))
  (set-cdr! x (cdr reversed)))
}

This looks like it will set @tt{x} to be the list made up of the
components of the reversed list. However, it does not do this.
First we set the first element of @tt{x} to be the first element
of the reversed list. However, this also changes the value at the
end of the reversed list, making it @tt{'(c b d)}! And then setting
the rest of @tt{x} to be the rest of the reversed list, where the last
element of the reversed list is actually @tt{x}, will create a cycle.

@bold{TODO: Can we actually solve this?}

@section[#:tag "c3e15"]{Exercise 3.15}

@bold{TODO: Art}

@section[#:tag "c3e16"]{Exercise 3.16}

@bold{I'm not sure I understand what this question is asking. TODO}

@section[#:tag "c3e17"]{Exercise 3.17}

@bold{TODO}

@section[#:tag "c3e18"]{Exercise 3.18}

Detecting cycles can be done simply. We can traverse the list and keep a
list of all the entries we've seen so far, and if we ever run into a node
that's already in the list of things we've seen, then we know we have a cycle.

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (detect-cycle l)
   (define (update l seen)
     (cond ((null? l) #f)
           ((contains seen l) #t)
           (else
            (update (cdr l) (cons l seen)))))
   (update l '()))
 ]

One thing to notice about this procedure is that the @tt{seen} list contains
references to entire lists, and not just the @tt{car}s of the lists. This is
important: We really want to see if we pass by an entire list we've already
seen before, not just a value we've seen before. Otherwise, the list
@tt{'(1 2 1 2)} would contain a cycle.

The @tt{contains} procedure is mostly trivial. Notice how it compares
@tt{(car lst)} to @tt{v} -- this is because every entry in the @tt{seen}
list is in fact a list, and @tt{(car lst)} gives us one of those lists.

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (contains lst v)
   (cond ((null? lst) #f)
         ((eq? (car lst) v) #t)
         (else
          (contains (cdr lst) v))))
 ]

@section[#:tag "c3e19"]{Exercise 3.19}

We can detect cycles in constant space by using two pointers into the list.
The basic idea is that we have the two pointers traverse the list at
different speeds -- one moving forward one entry at a time, and the other
by two. At some point, if the list contains a cycle, these two pointers
will point to the same thing. Alternatively, if a pointer reaches the end
of the list before this happens, we know we don't have a cycle.

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (tortoise-hare l)
   (define (loop tortoise hare)
     (cond ((or (null? tortoise)
                (null? hare))
            #f)
           ((eq? tortoise hare) #t)
           (else
            (let ((t2 (cdr tortoise))
                  (h2 (cdr hare)))
              (if (null? h2)
                  (loop t2 h2)
                  (loop t2 (cdr h2)))))))
   (loop l (cdr l)))
 ]

@section[#:tag "c3e20"]{Exercise 3.20}

@bold{TODO}

@section[#:tag "c3e21"]{Exercise 3.21}

@examples[
 #:hidden
 #:eval ev
 (define (front-ptr queue) (car queue))
 (define (rear-ptr queue) (cdr queue))
 (define (set-front-ptr! queue item) (set-car! queue item))
 (define (set-rear-ptr! queue item) (set-cdr! queue item))
 (define (empty-queue? queue) (null? (front-ptr queue)))
 (define (make-queue) (cons '() '()))
 (define (front-queue queue)
   (if (empty-queue? queue)
       (error "FRONT called with an empty queue" queue)
       (car (front-ptr queue))))
 (define (insert-queue! queue item)
   (let ((new-pair (cons item '())))
     (cond ((empty-queue? queue)
            (set-front-ptr! queue new-pair)
            (set-rear-ptr! queue new-pair)
            queue)
           (else
            (set-cdr! (rear-ptr queue) new-pair)
            (set-rear-ptr! queue new-pair)
            queue))))
 (define (delete-queue! queue)
   (cond ((empty-queue? queue)
          (error "DELETE! called with an empty queue" queue))
         (else
          (set-front-ptr! queue (cdr (front-ptr queue)))
          queue)))
 ]
Ben expects the queue to be printed like a list, with the front
item of the queue first and the rear item last. However, the queue
data structure is not a list: It is merely a pair of pointers to
lists. The REPL prints this as a pair, with the first entry being
the list starting at the front pointer and the second being the list
starting from the rear pointer (which only has one element). Deleting
the first element of a two-element queue will make the front pointer
and the rear pointer both point to a list of one item, for example.

Additionally, Ben is confused because removing all the entries from
the queue still leaves the rear pointer pointing to @tt{'b}. This
is because the implementation of @tt{delete-queue!} doesn't bother
to clear the rear pointer, knowing that the emptiness of the queue
is checked by only using the front pointer. This is reasonable,
although it will have consequences for garbage collection.

However, printing the queue as a list is actually easy, since the
items in the queue are connected to each other in a list structure.
All you have to do is print the list starting from the front pointer.

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (print-queue queue)
   (display (car queue))
   (newline))
 ]

It would perhaps be more useful to return the queue, since this might
be useful for other purposes and the REPL will print the result anyway,
but this is completely trivial (even more than the above was).

@section[#:tag "c3e22"]{Exercise 3.22}

Implementing a queue as a mutable object is not difficult. The internal
procedures are almost exactly the same, except they no longer take a queue
as a parameter, because they're internal to the queue now. Additionally,
the @tt{front-ptr} and @tt{rear-ptr} procedures are no longer needed because
these are stored as internal definitions that can be accessed directly.

One thing to note is that all of the calls to @tt{dispatch} return procedures
-- even if the procedure returned takes no argument.

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (make-queue)
   (let ((front-ptr '())
         (rear-ptr '()))
     (define (empty-queue?)
       (null? front-ptr))
     (define (front-queue)
       (if (empty-queue?)
           (error "FRONT called with an empty queue")
           (car front-ptr)))
     (define (insert-queue! item)
       (let ((new-pair (cons item '())))
         (cond ((empty-queue?)
                (set! front-ptr new-pair)
                (set! rear-ptr new-pair))
               (else
                (set-cdr! rear-ptr new-pair)
                (set! rear-ptr new-pair)))))
     (define (delete-queue!)
       (cond ((empty-queue?)
              (error "DELETE! called with an empty queue"))
             (else
              (set! front-ptr (cdr front-ptr)))))
     (define (dispatch m)
       (cond ((eq? m 'empty-queue?)
              empty-queue?)
             ((eq? m 'front-queue)
              front-queue)
             ((eq? m 'insert-queue!)
              insert-queue!)
             ((eq? m 'delete-queue!)
              delete-queue!)))
     dispatch))
 ]

@section[#:tag "c3e23"]{Exercise 3.23}

In order to make a deque with @tt{O(1)} insertion and deletion operations,
we need to use a new data structure: A doubly-linked list. This allows us
to add and remove from the deque at both ends in constant time. Otherwise,
for example, it would be impossible to remove the item at the rear of
the queue without traversing through the entire deque to get a pointer
to the item before it.

I've chosen to represent a doubly-linked list as a list of lists. Each inner
list represents a node, and has three values: The value of the node, the
pointer to the next item in the list, and the pointer to the previous
item in the list. Nodes on the ends of the deque will have @tt{nil} pointers
in one (or both) of these places.

I've chosen to implement a deque as a mutable object with pointers to the
front and rear, like in the last example. The code is not especially difficult,
but it is a bit longer than usual examples.

There are a few things to note about it, in particular the special cases that
occur when adding nodes to an empty deque or removing the last node. However,
there is a symmetry between the operations that occur at the front and rear
ends of the deque.

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (make-deque)
   (let ((front-ptr '()))
     (let ((rear-ptr front-ptr))
       (define (set-next-ptr from to)
         (set-car! (cdr from) to))
       (define (set-prev-ptr from to)
         (set-car! (cddr from) to))
       (define (empty-deque?)
         (null? front-ptr))
       (define (front-deque)
         (if (empty-deque?)
             (error "FRONT-DEQUE called on empty deque")
             (car front-ptr)))
       (define (rear-deque)
         (if (empty-deque?)
             (error "REAR-DEQUE called on empty deque")
             (car rear-ptr)))
       (define (front-insert-deque! item)
         (let ((new-item (list item front-ptr nil)))
           (if (empty-deque?)
               (begin
                 (set! front-ptr new-item)
                 (set! rear-ptr new-item))
               (begin
                 (set-prev-ptr front-ptr new-item)
                 (set! front-ptr new-item)))))
       (define (rear-insert-deque! item)
         (let ((new-item (list item nil rear-ptr)))
           (if (empty-deque?)
               (begin
                 (set! front-ptr new-item)
                 (set! rear-ptr new-item))
               (begin
                 (set-next-ptr rear-ptr new-item)
                 (set! rear-ptr new-item)))))
       (define (front-delete-deque!)
         (if (empty-deque?)
             (error "FRONT-DELETE-DEQUE! called on empty deque")
             (begin
               (set! front-ptr (cadr front-ptr))
               (if (null? front-ptr)
                   (set! rear-ptr front-ptr)
                   (set-prev-ptr front-ptr nil)))))
       (define (rear-delete-deque!)
         (if (empty-deque?)
             (error "REAR-DELETE-DEQUE! called on empty deque")
             (begin
               (set! rear-ptr (caddr rear-ptr))
               (if (null? rear-ptr)
                   (set! front-ptr rear-ptr)
                   (set-next-ptr rear-ptr nil)))))
       (define (dispatch m)
         (cond ((eq? m 'empty-deque?) empty-deque?)
               ((eq? m 'front-deque) front-deque)
               ((eq? m 'rear-deque) rear-deque)
               ((eq? m 'front-insert-deque!) front-insert-deque!)
               ((eq? m 'rear-insert-deque!) rear-insert-deque!)
               ((eq? m 'front-delete-deque!) front-delete-deque!)
               ((eq? m 'rear-delete-deque!) rear-delete-deque!)
               (else
                (error "unknown message" m))))
       dispatch)))
 ]

At this point, I think it should be mentioned that I dislike using the
calling conventions for these mutable data objects. I prefer dealing
with functions that take objects as parameters. However, it isn't difficult
to create those functions on top of an object like this.

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (empty-deque? dq)
   ((dq 'empty-deque?)))
 
 (define (front-deque dq)
   ((dq 'front-deque)))
 
 (define (rear-deque dq)
   ((dq 'rear-deque)))
 
 (define (front-insert-deque! dq item)
   ((dq 'front-insert-deque!) item))
 
 (define (rear-insert-deque! dq item)
   ((dq 'rear-insert-deque!) item))
 
 (define (front-delete-deque! dq)
   ((dq 'front-delete-deque!)))
 
 (define (rear-delete-deque! dq)
   ((dq 'rear-delete-deque!)))
 ]

@section[#:tag "c3e24"]{Exercise 3.24}

This exercise is pretty easy given the implementation of @tt{make-table}
on the previous page. We just need to create a generic @tt{assoc} procedure
that can be called within our new @tt{make-table} variant.

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (generic-assoc key records same-key?)
   (cond ((null? records) false)
         ((same-key? key (caar records)) (car records))
         (else (generic-assoc key (cdr records) same-key?))))
 
 (define (make-table same-key?)
   (let ((assoc (lambda (key records) (generic-assoc key records same-key?)))
         (local-table (list '*table*)))
     (define (lookup key-1 key-2)
       (let ((subtable (assoc key-1 (cdr local-table))))
         (if subtable
             (let ((record (assoc key-2 (cdr subtable))))
               (if record
                   (cdr record)
                   false))
             false)))
     (define (insert! key-1 key-2 value)
       (let ((subtable (assoc key-1 (cdr local-table))))
         (if subtable
             (let ((record (assoc key-2 (cdr subtable))))
               (if record
                   (set-cdr! record value)
                   (set-cdr! subtable
                             (cons (cons key-2 value)
                                   (cdr subtable)))))
             (set-cdr! local-table
                       (cons (list key-1
                                   (cons key-2 value))
                             (cdr local-table)))))
       'ok)
     (define (dispatch m)
       (cond ((eq? m 'lookup-proc) lookup)
             ((eq? m 'insert-proc!) insert!)
             (else (error "Unknown operation -- TABLE" m))))
     dispatch))
 ]

@section[#:tag "c3e25"]{Exercise 3.25}

To generalize tables beyond one and two dimensional keys, we
can imagine that a table entry is either a table or a key.
When performing a lookup, we should expect to find a value
only after looking up every one of the keys we were supplied
with. If a key is not present or if a lookup evaluates to a
value before exhausting the given keys, a lookup is said to
have failed. If the list of keys is exhausted before finding
a value, I believe returning the subtable is the best thing
to do.

Because I am going to be returning tables in addition to
values, I believe it is better to define @tt{lookup} and
@tt{insert} as functions taking tables rather than
constructing the tables as objects containing these
methods. However, I am going to construct these methods
differently than we did previously, by making them take
advantage of partial application to make the use of these
functions easier when operating on the same table.

@bold{TODO: Look more carefully at this}

@examples[       
 #:label #f #:eval ev #:no-prompt
 (define (lookup table)
   (lambda (keys)
     (if (null? keys) table
         (let ((record (assoc (car keys) (cdr table))))
           (if record
               ((lookup record) (cdr keys))
               false)))))
 ]

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (insert! table)
   (lambda (args)
     (if (not (null? args))
         (if (null? (cdr args))
             (set-cdr! table (car args))
             (let ((subtable (assoc (car args) (cdr table))))
               (if subtable
                   ((insert! subtable) (cdr args))
                   (let ((new-subtable (list (car args))))
                     ((insert! new-subtable) (cdr args))
                     (set-cdr! table (cons new-subtable (cdr table))))))))))
 ]

@section[#:tag "c3e26"]{Exercise 3.26}

Suppose at first that we will only allow keys to be numbers,
like in @secref{c2e66}. Besides slightly changing conventions to
match the ones we have now, the only new work we have to do is
adding an @tt{insert!} procedure to modify the binary tree.

The most significant change is that every tree root must also
contain a "dummy" first entry, so that we have a pointer to a
tree that we can use @tt{set-cdr!} on. This is accomplished by
using the @tt{make-table} procedure defined earlier to create
our empty trees (after all, if a tree is a table, then every
subtree is also a table). Other than this change, the logic is
almost identical, and the two procedures are almost identical.

@examples[
 #:hidden #:eval ev
 (define (entry tree) (car tree))
 (define (left-branch tree) (cadr tree))
 (define (right-branch tree) (caddr tree))
 
 (define (make-tree entry left right)
   (list entry left right))
 
 (define (make-table)
   (list '*table*))
 ]

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (lookup table)
   (lambda (key)
     (let ((entries (cdr table)))
       (if (null? entries)
           false
           (let ((e (entry entries)))
             (let ((entry-key (car e))
                   (entry-val (cdr e)))
               (cond ((= key entry-key) entry-val)
                     ((< key entry-key) ((lookup (left-branch entries)) key))
                     ((> key entry-key) ((lookup (right-branch entries)) key)))))))))
 
 (define (insert! table)
   (lambda (key value)
     (let ((entries (cdr table)))
       (if (null? entries)
           (set-cdr! table (make-tree (cons key value) (make-table) (make-table)))
           (let ((e (entry entries)))
             (let ((entry-key (car e)))
               (cond ((= key entry-key) (set-cdr! e value))
                     ((< key entry-key) ((insert! (left-branch entries)) key value))
                     ((> key entry-key) ((insert! (right-branch entries)) key value)))))))))
 ]

An example of this in use:

@examples[
 #:label #f #:eval ev
 (define t (make-table))
 (define lookup_t (lookup t))
 (define insert!_t (insert! t))
 (insert!_t 4 5)
 (lookup_t 4)
 (insert!_t 3 8)
 (insert!_t 2 1)
 (insert!_t 7 4)
 (lookup_t 3)
 (lookup_t 2)
 (lookup_t 7)
 (pretty-display t)
 ]


In order to store any orderable data type as a key in the
table, we must also supply an ordering function to these
operations. One way to do this is to make higher-order
functions that return @tt{lookup} and @tt{insert!} functions
with the ordering function bound. To make this easier to use,
we can standardize our comparator function to follow this format:

@verbatim{
cmp(a, b) = -1 if a < b
          |  0 if a = b
          |  1 if a > b
}

An implementation of generic tables is as follows:

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (make-lookup cmp)
   (let ((lt (lambda (a b) (= -1 (cmp a b))))
         (eq (lambda (a b) (= 0 (cmp a b))))
         (gt (lambda (a b) (= 1 (cmp a b)))))
     (lambda (table)
       (lambda (key)
         (let ((entries (cdr table)))
           (if (null? entries)
               false
               (let ((e (entry entries)))
                 (let ((entry-key (car e))
                       (entry-val (cdr e)))
                   (cond ((eq key entry-key) entry-val)
                         ((lt key entry-key) ((lookup (left-branch entries)) key))
                         ((gt key entry-key) ((lookup (right-branch entries)) key)))))))))))
 
 (define (make-insert! cmp)
   (let ((lt (lambda (a b) (= -1 (cmp a b))))
         (eq (lambda (a b) (= 0 (cmp a b))))
         (gt (lambda (a b) (= 1 (cmp a b)))))
     (lambda (table)
       (lambda (key value)
         (let ((entries (cdr table)))
           (if (null? entries)
               (set-cdr! table (make-tree (cons key value) (make-table) (make-table)))
               (let ((e (entry entries)))
                 (let ((entry-key (car e)))
                   (cond ((eq key entry-key) (set-cdr! e value))
                         ((lt key entry-key) ((insert! (left-branch entries)) key value))
                         ((gt key entry-key) ((insert! (right-branch entries)) key value)))))))))))
 ]

@section[#:tag "c3e27"]{Exercise 3.27}

@tt{memo-fib} is called recursively exponentially-many
times, but each of these calls will only do real work if the
call to @tt{lookup} doesn't return a previously-computed
result. After it has been computed, it is inserted in the
table to be found again. Because all @tt{memo-fib} calls
share the same table, and because new results can be computed
from old results in constant time, the overall running time
for @tt{memo-fib} is now linear.

It would @emph{not} have worked to define @tt{memo-fib} as 
@tt{(memoize (fib)} because the unmemoized @tt{fib}
function would be used to compute results. Only the direct
results of calling @tt{memo-fib} would be stored in the
table, so calling @tt{((memoize fib) 3)} would only store
the result for @tt{3} in the table, and not any of the
subproblem results. In other words, the algorithm would no
longer be able to run in linear time because it would not be
reusing all previously-computed results as you would expect
it to.

@section[#:tag "c3e28"]{Exercise 3.28}

@examples[
 #:hidden #:eval ev
 (define (make-queue) (cons '() '()))
 
 (define (make-wire)
   (let ((signal-value 0) (action-procedures '()))
     (define (set-my-signal! new-value)
       (if (not (= signal-value new-value))
           (begin (set! signal-value new-value)
                  (call-each action-procedures))
           'done))
     
     (define (accept-action-procedure! proc)
       (set! action-procedures (cons proc action-procedures))
       (proc))
     
     (define (dispatch m)
       (cond ((eq? m 'get-signal) signal-value)
             ((eq? m 'set-signal!) set-my-signal!)
             ((eq? m 'add-action!) accept-action-procedure!)
             (else (error "Unknown operation -- WIRE" m))))
     
     dispatch))
 
 (define (call-each procedures)
   (if (null? procedures)
       'done
       (begin
         ((car procedures))
         (call-each (cdr procedures)))))
 
 (define (get-signal wire)
   (wire 'get-signal))
 
 (define (set-signal! wire new-value)
   ((wire 'set-signal!) new-value))
 
 (define (add-action! wire action-procedure)
   ((wire 'add-action!) action-procedure))
 
 (define (after-delay delay action)
   (add-to-agenda! (+ delay (current-time the-agenda))
                   action
                   the-agenda))
 
 (define (propagate)
   (if (empty-agenda? the-agenda)
       'done
       (let ((first-item (first-agenda-item the-agenda)))
         (first-item)
         (remove-first-agenda-item! the-agenda)
         (propagate))))
 
 (define (probe name wire)
   (add-action! wire
                (lambda ()
                  (newline)
                  (display name)
                  (display " ")
                  (display (current-time the-agenda))
                  (display " New value = ")
                  (display (get-signal wire)))))
 
 (define (make-time-segment time queue)
   (cons time queue))
 
 (define (segment-time s) (car s))
 
 (define (segment-queue s) (cdr s))
 
 (define (make-agenda) (list 0))
 
 (define (current-time agenda) (car agenda))
 
 (define (set-current-time! agenda time)
   (set-car! agenda time))
 
 (define (segments agenda) (cdr agenda))
 
 (define (set-segments! agenda segments)
   (set-cdr! agenda segments))
 
 (define (first-segment agenda) (car (segments agenda)))
 
 (define (rest-segments agenda) (cdr (segments agenda)))
 
 (define (empty-agenda? agenda)
   (null? (segments agenda)))
 
 (define (add-to-agenda! time action agenda)
   (define (belongs-before? segments)
     (or (null? segments)
         (< time (segment-time (car segments)))))
   (define (make-new-time-segment time action)
     (let ((q (make-queue)))
       (insert-queue! q action)
       (make-time-segment time q)))
   (define (add-to-segments! segments)
     (if (= (segment-time (car segments)) time)
         (insert-queue! (segment-queue (car segments))
                        action)
         (let ((rest (cdr segments)))
           (if (belongs-before? rest)
               (set-cdr!
                segments
                (cons (make-new-time-segment time action)
                      (cdr segments)))
               (add-to-segments! rest)))))
   (let ((segments (segments agenda)))
     (if (belongs-before? segments)
         (set-segments!
          agenda
          (cons (make-new-time-segment time action)
                segments))
         (add-to-segments! segments))))
 
 (define (remove-first-agenda-item! agenda)
   (let ((q (segment-queue (first-segment agenda))))
     (delete-queue! q)
     (if (empty-queue? q)
         (set-segments! agenda (rest-segments agenda)))))
 
 (define (first-agenda-item agenda)
   (if (empty-agenda? agenda)
       (error "Agenda is empty -- FIRST-AGENDA-ITEM")
       (let ((first-seg (first-segment agenda)))
         (set-current-time! agenda (segment-time first-seg))
         (front-queue (segment-queue first-seg)))))
 
 (define inverter-delay 2)
 
 (define (inverter input output)
   (define (invert-input)
     (let ((new-value (logical-not (get-signal input))))
       (after-delay inverter-delay
                    (lambda ()
                      (set-signal! output new-value)))))
   (add-action! input invert-input)
   'ok)
 
 (define (logical-not s)
   (cond ((= s 0) 1)
         ((= s 1) 0)
         (else (error "Invalid signal" s))))
 
 (define and-gate-delay 3)
 
 (define (and-gate a1 a2 output)
   (define (and-action-procedure)
     (let ((new-value
            (logical-and (get-signal a1) (get-signal a2))))
       (after-delay and-gate-delay
                    (lambda ()
                      (set-signal! output new-value)))))
   (add-action! a1 and-action-procedure)
   (add-action! a2 and-action-procedure)
   'ok)
 
 (define (logical-and a b)
   (cond ((and (= a 0) (= b 0)) 0)
         ((and (= a 1) (= b 0)) 0)
         ((and (= a 0) (= b 1)) 0)
         ((and (= a 1) (= b 1)) 1)
         (else (error "Invalid signals" a b))))
 
 (define (half-adder a b s c)
   (let ((d (make-wire)) (e (make-wire)))
     (or-gate a b d)
     (and-gate a b c)
     (inverter c e)
     (and-gate d e s)
     'ok))
 
 (define (full-adder a b c-in sum c-out)
   (let ((s (make-wire))
         (c1 (make-wire))
         (c2 (make-wire)))
     (half-adder b c-in s c1)
     (half-adder a s sum c2)
     (or-gate c1 c2 c-out)
     'ok))
 ]

Implementing @tt{or-gate} by copying @tt{and-gate} is fairly trivial:

@examples[
 #:label #f #:eval ev #:no-prompt
 (define or-gate-delay 5)
 
 (define (or-gate o1 o2 output)
   (define (or-action-procedure)
     (let ((new-value (logical-or (get-signal o1) (get-signal o2))))
       (after-delay or-gate-delay
                    (lambda () (set-signal! output new-value)))))
   (add-action! o1 or-action-procedure)
   (add-action! o2 or-action-procedure)
   'ok)
 
 (define (logical-or a b)
   (cond ((and (= a 0) (= b 0)) 0)
         ((and (= a 1) (= b 0)) 1)
         ((and (= a 0) (= b 1)) 1)
         ((and (= a 1) (= b 1)) 1)
         (else (error "Invalid signals" a b))))
 ]

The fact that this is so similar to @tt{and-gate} is a clue
that a more general function is hiding -- in this case, that
of the binary operation:

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (binary-op op delay)
   (lambda (x y output)
     (define (binary-action-procedure)
       (let ((new-value (op (get-signal x) (get-signal y))))
         (after-delay delay
                      (lambda () (set-signal! output new-value)))))
     (add-action! x binary-action-procedure)
     (add-action! y binary-action-procedure)
     'ok))
 
 (define and-gate (binary-op logical-and and-gate-delay))
 (define or-gate (binary-op logical-or or-gate-delay))
 ]

@section[#:tag "c3e29"]{Exercise 3.29}

Using De Morgan's law, we know that a logical or can be
computed by taking the negation of the logical and of two
negated inputs.

@examples[
#:label #f #:eval ev #:no-prompt
 (define (or-compound A B output)
   (let ((nA (make-wire))
         (nB (make-wire))
         (nO (make-wire)))
     (inverter A nA)
     (inverter B nB)
     (and-gate nA nB nO)
     (inverter nO output)))              
 ]

The delay of this component is equal to the delay of the one
inverter that each of the inputs travel through plus the
delay of the and-gate and plus the delay of the final
inverter, or @tt{2 * inverter-delay + and-gate-delay}.

@section[#:tag "c3e30"]{Exercise 3.30}

Constructing a ripple-carry adder is essentially about
zipping corresponding elements in the lists into
full-adders. However, the condition that the output carry
signal from one full-adder is used as the input carry signal
to the next complicates things slightly, as does the
requirement that the final input wire be set to @tt{0}.

Working around these problems, I use a recursive procedure
that uses the first and second carry wires as the input and
output carry signals, respectively. Constructing the
ripple-carry adder from the left, since there is one more
carry wire than the others, once the other wires are
expended, we have the input carry wire that we need to
set to @tt{0} -- this making for a good base case for our
recursion.

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (ripple-carry-adder As Bs Ss C)
   (define Cs (map (lambda (_) (make-wire))))
   (define (make-adder as bs cs ss)
     (if (null? as)
         (set-signal! (car cs) 0)
         (begin
           (full-adder (car as)
                       (car bs)
                       (cadr cs)
                       (car ss)
                       (car cs))
           (make-adder (cdr as) (cdr bs) (cdr cs) (cdr ss)))))
   (make-adder As Bs (cons C Cs) Ss))
 ]

@section[#:tag "c3e31"]{Exercise 3.31}

@examples[
 #:hidden #:eval ev
 (define the-agenda (make-agenda))
 ]

This is the interactive session that we are expecting to see,
with @tt{accept-action-procedure} immediately running the
procedure after adding it ot the list:

@examples[
 #:label #f #:eval ev
 (define input-1 (make-wire))
 (define input-2 (make-wire))
 (define sum (make-wire))
 (define carry (make-wire))
 
 (probe 'sum sum)
 
 (probe 'carry carry)

 (half-adder input-1 input-2 sum carry)

 (set-signal! input-1 1)
 (propagate)

 (set-signal! input-2 1)
 (propagate)
 ]

If we change the definition to not do this, we get this session:

@examples[
 #:label #f #:eval ev #:no-prompt
   (define (make-wire-bad)
   (let ((signal-value 0) (action-procedures '()))
     (define (set-my-signal! new-value)
       (if (not (= signal-value new-value))
           (begin (set! signal-value new-value)
                  (call-each action-procedures))
           'done))
     
     (define (accept-action-procedure! proc)
       (set! action-procedures (cons proc action-procedures)))
     
     (define (dispatch m)
       (cond ((eq? m 'get-signal) signal-value)
             ((eq? m 'set-signal!) set-my-signal!)
             ((eq? m 'add-action!) accept-action-procedure!)
             (else (error "Unknown operation -- WIRE" m))))
     
     dispatch))
   ]

@examples[
 #:label #f #:eval ev
 (define the-agenda (make-agenda))
 
 (define input-1 (make-wire-bad))
 (define input-2 (make-wire-bad))
 (define sum (make-wire-bad))
 (define carry (make-wire-bad))
 
 (probe 'sum sum)
 
 (probe 'carry carry)

 (half-adder input-1 input-2 sum carry)

 (set-signal! input-1 1)
 (propagate)

 (set-signal! input-2 1)
 (propagate)
 ]

Because action procedures are not run after being added to
wires, the initial values of the wires are not propagated,
meaning that later calculations are not done corrrectly.

@section[#:tag "c3e32"]{Exercise 3.32}

Suppose that we have an and gate whose inputs change from 
@tt{(0, 1)} to @tt{(1, 0)} in the same time segment. This
means that two procedures setting the output value of the
gate are added to the agenda. In the first of these procedures,
only the first input has changed -- only the second procedure
gives us an answer consistent with the state of the inputs.

However, under our new supposition that the procedures in a
time segment are stored in a list and added to and removed
from the front (i.e. with stack semantics), the second
procedure will be run first, and the (correct) answer that
it assigned to the output wire will be overwritten by the
inconsistent answer from the first procedure.

The only way to guarantee that the state of the circuit
stays correct after every time segment in the agenda is to
run procedures in them in the order in which they were
added.

@section[#:tag "c3e33"]{Exercise 3.33}

@examples[
 #:hidden #:eval ev
 (define (adder a1 a2 sum)
   (define (process-new-value)
     (cond ((and (has-value? a1) (has-value? a2))
            (set-value! sum
                        (+ (get-value a1) (get-value a2))
                        me))
           ((and (has-value? a1) (has-value? sum))
            (set-value! a2
                        (- (get-value sum) (get-value a1))
                        me))
           ((and (has-value? a2) (has-value? sum))
            (set-value! a1
                        (- (get-value sum) (get-value a2))
                        me))))
   (define (process-forget-value)
     (forget-value! sum me)
     (forget-value! a1 me)
     (forget-value! a2 me)
     (process-new-value))
   (define (me request)
     (cond ((eq? request 'I-have-a-value)  
            (process-new-value))
           ((eq? request 'I-lost-my-value) 
            (process-forget-value))
           (else 
            (error "Unknown request -- ADDER" request))))
   (connect a1 me)
   (connect a2 me)
   (connect sum me)
   me)
 (define (inform-about-value constraint)
   (constraint 'I-have-a-value))
 (define (inform-about-no-value constraint)
   (constraint 'I-lost-my-value))
 (define (multiplier m1 m2 product)
   (define (process-new-value)
     (cond ((or (and (has-value? m1) (= (get-value m1) 0))
                (and (has-value? m2) (= (get-value m2) 0)))
            (set-value! product 0 me))
           ((and (has-value? m1) (has-value? m2))
            (set-value! product
                        (* (get-value m1) (get-value m2))
                        me))
           ((and (has-value? product) (has-value? m1))
            (set-value! m2
                        (/ (get-value product) (get-value m1))
                        me))
           ((and (has-value? product) (has-value? m2))
            (set-value! m1
                        (/ (get-value product) (get-value m2))
                        me))))
   (define (process-forget-value)
     (forget-value! product me)
     (forget-value! m1 me)
     (forget-value! m2 me)
     (process-new-value))
   (define (me request)
     (cond ((eq? request 'I-have-a-value)
            (process-new-value))
           ((eq? request 'I-lost-my-value)
            (process-forget-value))
           (else
            (error "Unknown request -- MULTIPLIER" request))))
   (connect m1 me)
   (connect m2 me)
   (connect product me)
   me)
 (define (constant value connector)
   (define (me request)
     (error "Unknown request -- CONSTANT" request))
   (connect connector me)
   (set-value! connector value me)
   me)
 (define (probe name connector)
   (define (print-probe value)
     (newline)
     (display "Probe: ")
     (display name)
     (display " = ")
     (display value))
   (define (process-new-value)
     (print-probe (get-value connector)))
   (define (process-forget-value)
     (print-probe "?"))
   (define (me request)
     (cond ((eq? request 'I-have-a-value)
            (process-new-value))
           ((eq? request 'I-lost-my-value)
            (process-forget-value))
           (else
            (error "Unknown request -- PROBE" request))))
   (connect connector me)
   me)
 (define (make-connector)
   (let ((value false) (informant false) (constraints '()))
     (define (set-my-value newval setter)
       (cond ((not (has-value? me))
              (set! value newval)
              (set! informant setter)
              (for-each-except setter
                               inform-about-value
                               constraints))
             ((not (= value newval))
              (error "Contradiction" (list value newval)))
             (else 'ignored)))
     (define (forget-my-value retractor)
       (if (eq? retractor informant)
           (begin (set! informant false)
                  (for-each-except retractor
                                   inform-about-no-value
                                   constraints))
           'ignored))
     (define (connect new-constraint)
       (if (not (memq new-constraint constraints))
           (set! constraints 
                 (cons new-constraint constraints)))
       (if (has-value? me)
           (inform-about-value new-constraint))
       'done)
     (define (me request)
       (cond ((eq? request 'has-value?)
              (if informant true false))
             ((eq? request 'value) value)
             ((eq? request 'set-value!) set-my-value)
             ((eq? request 'forget) forget-my-value)
             ((eq? request 'connect) connect)
             (else (error "Unknown operation -- CONNECTOR"
                          request))))
     me))
 (define (for-each-except exception procedure list)
   (define (loop items)
     (cond ((null? items) 'done)
           ((eq? (car items) exception) (loop (cdr items)))
           (else (procedure (car items))
                 (loop (cdr items)))))
   (loop list))
 (define (has-value? connector)
   (connector 'has-value?))
 (define (get-value connector)
   (connector 'value))
 (define (set-value! connector new-value informant)
   ((connector 'set-value!) new-value informant))
 (define (forget-value! connector retractor)
   ((connector 'forget) retractor))
 (define (connect connector new-constraint)
   ((connector 'connect) new-constraint))
 ]

An averager is fairly easy to define:

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (averager a b c)
   (let ((d (make-connector))
         (e (make-connector)))
     (adder a b e)
     (multiplier c d e)
     (constant 2 d)
     'ok))
 ]

Used in a REPL session, it looks like this:

@examples[
 #:label #f #:eval ev
 (define A (make-connector))
 (define B (make-connector))
 (define C (make-connector))
 (averager A B C)
 (probe "A" A)
 (probe "B" B)
 (probe "C" C)
 (set-value! A 3 'user)
 (set-value! B 10 'user)
 (forget-value! A 'user)
 (set-value! C 8 'user)
 ]

@section[#:tag "c3e34"]{Exercise 3.34}

Let's try what Louis suggests for ourselves:

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (squarer a b)
   (multiplier a a b))
 ]

@examples[
 #:label #f #:eval ev
 (define A (make-connector))
 (define B (make-connector))
 (squarer A B)
 (probe "A" A)
 (probe "B" B)
 (set-value! A 2 'user)
 (forget-value! A 'user)
 (set-value! B 25 'user)
 ]

The problem here is that, although it is possible to square
the value in @tt{A} in order to derive @tt{B}, we do not
have a mechanism to do the reverse, because both of the
multiplicands are the same -- if we forget one of them, we
forget them both, and can't get anywhere anymore.

@section[#:tag "c3e35"]{Exercise 3.35}

If we're going to define a squarer, we're going to have
to invent a new primitive that knows what to do when
either one of the connectors has no value. We can
complete the definition of this primitive started in
the exercise like this:

@examples[
 #:hidden #:eval ev
 (define (square a) (* a a))
 ]

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (squarer a b)
   (define (process-new-value)
     (cond ((has-value? b)
            (if (< (get-value b) 0)
                (error "square less than 0 -- SQUARER" (get-value b))
                (set-value! a (sqrt (get-value b)) me)))
           ((has-value? a)
            (set-value! b (square (get-value a)) me))))
   (define (process-forget-value)
     (forget-value! a me)
     (forget-value! b me)
     (process-new-value))
   (define (me request)
     (cond ((eq? request 'I-have-a-value)
            (process-new-value))
           ((eq? request 'I-lost-my-value)
            (process-forget-value))
           (else
            (error "Unknown request -- SQUARER" request))))
   (connect a me)
   (connect b me)
   me)
 ]

The same REPL session as before shows that this works correctly:

@examples[
 #:label #f #:eval ev
 (define A (make-connector))
 (define B (make-connector))
 (squarer A B)
 (probe "A" A)
 (probe "B" B)
 (set-value! A 2 'user)
 (forget-value! A 'user)
 (set-value! B 25 'user)
 ]

@section[#:tag "c3e36"]{Exercise 3.36}

A crude diagram:

@verbatim{
global: (
 a: (
   [we are here]
   value: 10,
   informant: 'user,
   constraints: '()
 ),
 b: (...),
 inform-about-value: (...),
 for-each-except: (...),
 ...
)
}

@section[#:tag "c3e37"]{Exercise 3.37}

This is what we are given as an example of a
more expressive procedure for creating adder
connectors:

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (c+ x y)
   (let ((z (make-connector)))
     (adder x y z)
     z))
 ]

Note that it only takes the two input wires,
creating the output wire itself and returning
it as a result for more easy composition. We
can do similarly for the other operators:

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (c- x y)
   (let ((z (make-connector)))
     (adder y z x)
     z))
 (define (c* x y)
   (let ((z (make-connector)))
     (multiplier x y z)
     z))
 (define (c/ x y)
   (let ((z (make-connector)))
     (multiplier y z x)
     z))
 (define (cv v)
   (let ((z (make-connector)))
     (constant v z)
     z))
 ]

We can then define a Celsius-Fahrenheit converter as they
have. Note that, due to the semantics of how @tt{c/} is used
below, @tt{y} had to be used as an argument to
@tt{multiplier} before @tt{x}.

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (celsius-fahrenheit-converter x)
   (c+ (c* (c/ (cv 9) (cv 5))
           x)
       (cv 32)))
 (define C (make-connector))
 (define F (celsius-fahrenheit-converter C))
 ]

A REPL session shows that this works:

@examples[
 #:label #f #:eval ev
 (probe "F" F)
 (probe "C" C)
 (set-value! F 32 'user)
 (forget-value! F 'user)
 (set-value! C 100 'user)
 (forget-value! C 'user)
 (set-value! F 75 'user)
 ]

@section[#:tag "c3e38"]{Exercise 3.38}

If the transactions must run sequentially in
some order, then we can enumerate all of these
orderings to find all of the possible values
of @tt{balance}. Better yet, we can write a
program to do it for us.

@examples[
 #:hidden #:eval ev
 (define (accumulate op initial sequence)
   (if (null? sequence)
       initial
       (op (car sequence)
           (accumulate op initial (cdr sequence)))))
 ]

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (peter balance) (+ balance 10))
 (define (paul balance) (- balance 20))
 (define (mary balance) (- balance (/ balance 2)))

 (define permutations
   (list
    (list peter paul mary)
    (list peter mary paul)
    (list paul peter mary)
    (list paul mary peter)
    (list mary peter paul)
    (list mary paul peter)))

  (define (run-permutation p)
    (accumulate (lambda (proc val) (apply proc (list val))) 100 p))

  (pretty-display (map run-permutation permutations))
 ]

Things can become more complicated if the transactions
don't run atomically. For example, Peter's transaction
adding $10 could start such that it has calculated the
new balance but not updated it. If it yielded and allowed
both of the other transactions to run to completion, it
would set the final balance to be equal to the $110 it
would have been at the beginning. This would be great for
Peter, Paul, and Mary, but the bank would probably fire the
programmer who allowed it to happen.

@section[#:tag "c3e39"]{Exercise 3.39}

@codeblock{
(define x 10)

(define s (make-serializer))

(parallel-execute (lambda () (set! x ((s (lambda () (* x x))))))
                  (s (lambda () (set! x (+ x 1)))))
}

These possibilities can occur:

@itemlist[
 @item{@tt{101}: The first procedure runs to completion before the second}
 @item{@tt{100}: The calculation of @tt{(* x x)} is run first, followed by the
       incrementing of @tt{x} and then the setting of @tt{x} to @tt{100}}
 @item{@tt{121}: The second procedure runs to completion before the first}
 ]

@section[#:tag "c3e40"]{Exercise 3.40}

@codeblock{
(define x 10)

(parallel-execute (lambda () (set! x (* x x)))
                  (lambda () (set! x (* x x x))))
}

The different outcomes will occur because of the different places
that the value of @tt{x} can be changed, depending on which procedure
finishes first and at what point it does. The following can happen:

@itemlist[
 @item{@tt{1000000}: The first procedure finishes first, resulting in
  @tt{(* 100 100 100)}}
 @item{@tt{100000}: The first procedure finishes after one read in the second,
  resulting in @tt{(* 10 100 100)}}
 @item{@tt{10000}: The first procedure finishes after two reads in the second,
  resulting in @tt{(* 10 10 100)}}
 @item{@tt{1000}: The first procedure sets @tt{x} after the second procedure
  calculates the new value of @tt{x}}
 @item{@tt{1000000}: The second procedure finishes first, resulting in
  @tt{(* 1000 1000)} (note that this is the same as the first case)}
 @item{@tt{10000}: The second procedure finishes after one read in the first,
  resulting in @tt{(* 10 1000)}}
 @item{@tt{100}: The second procedure sets @tt{x} after the first procedure
  calculates the new value of @tt{x}}
 ]

Now suppose we use serialized procedures as such:

@codeblock{
(define x 10)

(define s (make-serializer))

(parallel-execute (s (lambda () (set! x (* x x))))
                  (s (lambda () (set! x (* x x x)))))
}

In this case, the only possible answer is @tt{1000000}, because the order
in which we perform the multiplications does not matter as long as they all
contribute to the answer. It doesn't matter which procedure runs first --
the answer will be the same.

@section[#:tag "c3e41"]{Exercise 3.41}

Suppose Ben Bitdiddle changes the definition of the bank account
to protect the viewing of the balance:

@codeblock{
 (define (make-account balance)
   (define (withdraw amount)
     (if (>= balance amount)
         (begin (set! balance (- balance amount))
                balance)
         "Insufficient funds"))
   (define (deposit amount)
     (set! balance (+ balance amount))
     balance)
   (let ((protected (make-serializer)))
     (define (dispatch m)
       (cond ((eq? m 'withdraw) (protected withdraw))
             ((eq? m 'deposit) (protected deposit))
             ((eq? m ' balance)
              ((protected (lambda () balance)))) ; serialized
             (else (error "Unknown request -- MAKE-ACCOUNT" m))))
     dispatch))
}

Since @tt{balance} is only accessed once during the processing of the
@tt{'balance} request, whether serializing it has any effect depends
on whether this operation is atomic.

If it is, then although it is possible to get an out of date answer if
a withdrawal or deposit occurs after the balance has been read but
before it has been reported to you, it's not clear that there is anything
wrong with this because the balance was correct at the time and is fully
consistent with the state of the account at some point in time.

If, however, reading the balance is not an atomic operation, then protecting
it will be worthwhile -- otherwise, it would be possible to read two
incomplete balances yielding an inconsitent total.

@section[#:tag "c3e42"]{Exercise 3.42}

Suppose Ben Bitdiddle now wants to serialize the withdraw and deposit
procedures outside of the definition of the dispatch function:

@codeblock{
 (define (make-account balance)
   (define (withdraw amount)
     (if (>= balance amount)
         (begin (set! balance (- balance amount))
                balance)
         "Insufficient funds"))
   (define (deposit amount)
     (set! balance (+ balance amount))
     balance)
   (let ((protected (make-serializer)))
     (let ((protected-withdraw (protected withdraw))
           (protected-deposit (protected deposit)))
       (define (dispatch m)
         (cond ((eq? m 'withdraw) (protected withdraw))
               ((eq? m 'deposit) (protected deposit))
               ((eq? m ' balance) balance)
               (else (error "Unknown request -- MAKE-ACCOUNT" m))))
       dispatch)))
}

This is a safe change to make. Performing multiple destructive updates
at the same time will still not be allowed, giving the same guarantees
without the performance impact of introducing a new procedure to the
serialization set every time such an update is made.

@section[#:tag "c3e43"]{Exercise 3.43}

It should be the case that any number of concurrent exchanges between
accounts should not violate the condition that each account always has
a balance that one of the accounts originally had, and that every unique
balance is represented once. This is essentially intuitively true: if the
accounts are only allowed to swap values with one another, it should not
be the case that any account ends up with a new value.

However, if the entire exchange is not serialized, this can happen. The
difference between accounts @tt{A} and @tt{B} can be calculated before
another transaction changes the balance in @tt{A}, meaning that the
withdrawal from @tt{A} will be withdrawing from a balance that is not
necessarily the same as it was when the difference was calculated. In
essence:

@verbatim{
a = 5
b = 10
c = 15

exchange(a, b):
  difference = 5 - 10
<-- set a to 15
  a = 15 + 5 = 20
  b = 10 - 5 = 5
}

If the entire exchange is serialized, as in the refined
example, the second exchange cannot interrupt the first,
making this situation impossible.

However, note that the sum of the balances is still
preserved -- that is, @tt{5 + 10 + 15 == 20 + 5 + 5}. This
is a simple consequence of the fact that the net effect on
the total balance of the accounts of an exchange is always
zero, as long as transactions on the individual accounts are
serialized. In other words, no new money is being introduced
into the system as long as you only exchange balances --
exchanging is a zero sum operation. If this were still not
the case, then any withdrawal or deposit could be
interrupted, causing the same sorts of problems we saw
before.

@section[#:tag "c3e44"]{Exercise 3.44}

Unlike @tt{exchange}, @tt{transfer} makes no calculations
based on the states of the balances before withdrawing or
depositing from them. Even if the balances in one or both of
the accounts were to change during the execution of the
procedure, as long as the withdrawals and deposits were
serialized, the net impact on both accounts will be correct.
This means that there is no need for more complex
serialization with the transfer operation.

@section[#:tag "c3e45"]{Exercise 3.45}

Louis Reasoner changes the definition of
@tt{make-account-and-serializer} to automatically serialize
deposits and withdrawals. Since @tt{serialized-exchange}
manually serializes the @tt{exchange} procedure with both
serializers, and the @tt{exchange} procedure will now call
serialized functions, a function called during the execution
of another function will be mutually exclusive -- in other
words, the procedure will not be able to terminate. This is
an example of deadlock.

Note that we don't have this problem when nesting two different serializers.

(This exercise anticipates the section on mutexes which directly follows.)

@section[#:tag "c3e46"]{Exercise 3.46}

The situation in which a non-atomic @tt{test-and-set!}
procedure fails is straightforward:

@itemlist[
 @item{One procedure attempts to open an available mutex}
 @item{After it determines that the mutex is available, but
  before it sets it, another procedure checks the value of
  the mutex and also determines that it is available}
 @item{Both procedures set the cell and return false, and
  both can execute at the same time}
 ]

@section[#:tag "c3e47"]{Exercise 3.47}

An implementation of semaphores in terms of mutexes can
proceed something like this:

A semaphore allows @tt{n} concurrently-executing procedures
at once. We can imagine checking this as equivalent to
checking if any of @tt{n} mutexes are unset.

@codeblock{
(define (make-semaphore n)
  (define (get-available-mutex ms)
    (cond ((null? ms) null)
          ((car ms) ((car ms) 'acquire)
          (else (get-available-mutex ms)))))
  (define (make-mutexes times)
    (if (= 0 times)
        null
        (cons (make-mutex) (make-mutexes (- times 1)))))
  (let ((mutexes (make-mutexes n)))
    (define (the-semaphore m)
      (cond
        ((eq? m 'acquire)
          (let ((acquired (get-available-mutex mutexes)))
            (if (null? acquired)
                (the-semaphore 'acquire)
                (lambda () (acquired 'release)))))
        (else (error "UNKNOWN MESSAGE -- " m))))
    the-semaphore))
}

As our convention, is a slot in the semaphore is acquired,
it returns a no-argument procedure that will release itself
when called, so that the owner can relinquish control.

We could also implement the same idea using
@tt{test-and-set!} calls. Note the essential similarity
between the two:

@codeblock{
 (define (make-semaphore n)
   (define (get-available-cell cells)
     (if (null? cells)
         null
         (let ((result (test-and-set! (car cells))))
           (if result
               (get-available-cell (cdr cells))
               (car cells)))))
   (define (make-cells times)
     (if (= 0 times)
         null
         (cons  (list false) (make-cells (- times 1)))))
   (let ((cells (make-cells n)))
     (define (the-semaphore m)
       (cond
         ((eq? m 'acquire)
           (let ((acquired (get-available-cell cells)))
             (if (null? acquired)
                 (the-semaphore 'acquire)
                 (lambda () (set-car! acquired false)))))
         (else (error "UNKNOWN MESSAGE -- " m))))
     the-semaphore))
}

@section[#:tag "c3e48"]{Exercise 3.48}

Suppose that @tt{serialized-exchange} will attempt to gain
access to a certain account over another, in this case by
numbering each account and mandating that we access the
lower-numbered account first. Now if two procedures attempt
to exchange between the two same accounts in reverse
positions, even though the lower-numbered account will be
withdrawn from in one procedure and deposited into in the
other, it will attempt to be accessed first regardless. This
means that only one procedure will be allowed to gain access
to the operations that need to be protected, and we can no
longer have the situation outlined in the text, where one
procedure gains access to account @tt{a1} and the other to
account @tt{a2} and neither can finish.

To implement this, we need to do the following:

@itemlist[
 @item{Expose a unique numeric identifier for each account}
 @item{Change @tt{serialized-exchange} to use the
  serializer for the lower-numbered account in the outer
  position}
 ]

@codeblock{
 (define global-id 0)
 (define (get-next-id!)
   (set! global-id (+ global-id 1))
   global-id)
 (define (make-account-and-serializer balance)
   (define (withdraw amount)
     (if (>= balance amount)
         (begin (set! balance (- balance amount))
                balance)
         "Insufficient funds"))
   (define (deposit amount)
     (set! balance (+ balance amount))
     balance)
   (let ((balance-serializer (make-serializer))
         (id (get-next-id!)))
     (define (dispatch m)
       (cond ((eq? m 'withdraw) withdraw)
             ((eq? m 'deposit) deposit)
             ((eq? m 'balance) balance)
             ((eq? m 'serializer) balance-serializer)
             ((eq? m 'id) id)
             (else (error "Unknown request -- MAKE-ACCOUNT"
                          m))))
     dispatch))
 (define (serialized-exchange account1 account2)
   (let ((id1 (account1 'id))
         (id2 (account2 'id))
         (serializer1 (account1 'serializer))
         (serializer2 (account2 'serializer)))
     (let ((first-serializer (if (< id1 id2) serializer1 serializer2))
           (second-serializer (if (< id1 id2) serializer2 serializer1)))
       ((serializer1 (serializer2 exchange))
        account1
        account2))))
}

@section[#:tag "c3e49"]{Exercise 3.49}

Suppose you wanted to write a program to swap the balances
of your account and the other account in the bank with the
balance closest to yours. Clearly, once you start examining
account balances, it would be problematic if your account
balance would change at any point before the transaction is
complete -- this could invalidate your search results,
making the end result of the program incorrect. But if you
need to lock your account before finding the other account
to match it, then you can no longer follow the procedure
above -- the matching account may have a higher or a lower
number, and if it has a lower one, you've violated the rules
that were attempting to prevent deadlock.

@section[#:tag "c3e50"]{Exercise 3.50}

This generalized version of @tt{stream-map} allows us to
pass a procedure @tt{proc} of @tt{n} arguments as well as
@tt{n} streams, and returns a stream containing @tt{proc}
applied to each of the @tt{i}th values of the streams.
It assumes that each stream is at least as long as the
first, and returns a stream of only as many elements as
there are elements of this. Note that, because @tt{argstreams}
is a list of streams, we can use normal list procedures
on it.

@examples[
 #:eval ev #:hidden
 (define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

 (define (stream-map proc s)
   (if (stream-null? s)
       the-empty-stream
       (cons-stream (proc (stream-car s))
                    (stream-map proc (stream-cdr s)))))
 
 (define (stream-for-each proc s)
   (if (stream-null? s)
       'done
       (begin (proc (stream-car s))
              (stream-for-each proc (stream-cdr s)))))
 
 (define (display-stream s)
   (stream-for-each display-line s))
 
 (define (display-line x)
   (newline)
   (display x))
 
 (define (stream-car stream) (car stream))
 (define (stream-cdr stream) (force (cdr stream)))
 
 (define (stream-enumerate-interval low high)
   (if (> low high)
       the-empty-stream
       (cons-stream
        low
        (stream-enumerate-interval (+ low 1) high))))
 
 (define (stream-filter pred stream)
   (cond ((stream-null? stream) the-empty-stream)
         ((pred (stream-car stream))
          (cons-stream (stream-car stream)
                       (stream-filter pred
                                      (stream-cdr stream))))
         (else (stream-filter pred (stream-cdr stream)))))
 
 (define (show x)
   (display-line x)
   x)
 ]

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (stream-map proc . argstreams)
   (if (stream-null? (car argstreams))
       the-empty-stream
       (cons-stream
        (apply proc (map stream-car argstreams))
        (apply stream-map
               (cons proc (map stream-cdr argstreams))))))
 ]

As an example,

@examples[
 #:label #f #:eval ev
 (display-stream (stream-map + (stream-enumerate-interval 1 10) (stream-enumerate-interval 1 1000000)))
 ]

@section[#:tag "c3e51"]{Exercise 3.51}

When we first define @tt{x}, @tt{stream-map} will call the
@tt{show} procedure on the first element of the stream,
which will be forced immediately. No other values will be
forced until the calling of @tt{(stream-ref x 5)}, which
will only force enough to find the sixth element. We
will then force two more when we call @tt{(stream-ref x 7)}.
We can see this in action:

@examples[
 #:label #f #:eval ev
 (define x (stream-map show (stream-enumerate-interval 0 10)))
 (stream-ref x 5)
 (stream-ref x 7)
 ]

@section[#:tag "c3e52"]{Exercise 3.52}

@examples[
 #:label #f #:eval ev
 (define sum 0)
 sum
 ]

Forced so far: None.

@examples[
 #:label #f #:eval ev
 (define (accum x)
   (set! sum (+ x sum))
   sum)
 sum
 ]

Forced so far: None.

@examples[
 #:label #f #:eval ev
 (define seq (stream-map accum (stream-enumerate-interval 1 20)))
 sum
 ]

Forced so far: @tt{1}.

@examples[
 #:label #f #:eval ev
 (define y (stream-filter even? seq))
 sum
 ]

Forced so far: @tt{1, 2, 3}.

@examples[
 #:label #f #:eval ev
 (define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                          seq))
 sum
 ]

Forced so far: @tt{1, 2, 3, 4}.

@examples[
 #:label #f #:eval ev
 (stream-ref y 7)
 sum
 ]

Forced so far: @tt{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16}.
@examples[
 #:label #f #:eval ev
 (display-stream z)
 sum
 ]

Forced so far: @tt{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20}.

@section[#:tag "c3e53"]{Exercise 3.53}

The stream defined by

@codeblock{
 (define s (cons-stream 1 (add-streams s s)))
}

is actually a definition of the powers of two. A proof by induction:

@itemlist[
 @item{The first element is @tt{1}, or @tt{2^0}}
 @item{Presume that the @tt{k}th element of @tt{s} is
  @tt{2^k}. The next element is @tt{2^k + 2^k}, which is
  equivalent to @tt{2*(2^k)} and to @tt{2^(k+1)}.}
 ]

@section[#:tag "c3e54"]{Exercise 3.54}

@examples[
 #:eval ev #:hidden
 (define (integers-starting-from n)
   (cons-stream n (integers-starting-from (+ n 1))))
 
 (define integers (integers-starting-from 1))
 
 (define (divisible? x y) (= (remainder x y) 0))
 
 (define no-sevens
   (stream-filter (lambda (x) (not (divisible? x 7)))
                  integers))
 
 (define (fibgen a b)
   (cons-stream a (fibgen b (+ a b))))
 
 (define fibs (fibgen 0 1))
 
 (define (sieve stream)
   (cons-stream
    (stream-car stream)
    (sieve (stream-filter
            (lambda (x)
              (not (divisible? x (stream-car stream)))))
           (stream-cdr stream))))
 
 (define ones (cons-stream 1 ones))
 
 (define (add-streams s1 s2)
   (stream-map + s1 s2))
 
 (define integers-implicitly (cons-stream 1 (add-streams ones integers-implicitly)))
 
 (define fibs-implicitly
   (cons-stream 0
                (cons-stream 1
                             (add-streams (stream-cdr fibs-implicitly)
                                          fibs-implicitly))))
 
 (define (scale-stream stream factor)
   (stream-map (lambda (x) (* x factor)) stream))
 
 (define double (cons-stream 1 (scale-stream double 2)))
 
 (define (square x) (* x x))
 
 (define primes
   (cons-stream
    2
    (stream-filter prime? (integers-starting-from 3))))
 
 (define (prime? n)
   (define (iter ps)
     (cond ((> (square (stream-car ps))  n) true)
           ((divisible? n (stream-car ps)) false)
           (else (iter (stream-cdr ps)))))
   (iter primes))
 ]

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (mul-streams s1 s2) (stream-map * s1 s2))
 (define factorials (cons-stream 1 (mul-streams factorials (integers-starting-from 2))))
 ]

To show that it works,
@examples[
 #:label #f #:eval ev
 (= (* 1 2 3 4 5 6 7 8 9 10 11) (stream-ref factorials 10))
 ]

@section[#:tag "c3e55"]{Exercise 3.55}

The definition of @tt{partial-sums} is almost identical to that of @tt{factorial}:

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (partial-sums s)
   (if (stream-null? s)
       the-empty-stream
       (cons-stream (stream-car s) (add-streams (partial-sums s) (stream-cdr s)))))
 ]

@section[#:tag "c3e56"]{Exercise 3.56}

@examples[
 #:eval ev #:hidden
 (define (merge s1 s2)
   (cond ((stream-null? s1) s2)
         ((stream-null? s2) s1)
         (else
          (let ((s1car (stream-car s1))
                (s2car (stream-car s2)))
            (cond ((< s1car s2car)
                   (cons-stream s1car (merge (stream-cdr s1) s2)))
                  ((> s1car s2car)
                   (cons-stream s2car (merge s1 (stream-cdr s2))))
                  (else
                   (cons-stream s1car
                                (merge (stream-cdr s1)
                                       (stream-cdr s2)))))))))
 ]

@examples[
 #:label #f #:eval ev #:no-prompt
 (define S (cons-stream 1 (merge (scale-stream S 2) (merge (scale-stream S 3) (scale-stream S 5)))))
 ]

@section[#:tag "c3e57"]{Exercise 3.57}

When computing @tt{fibs} using the definition presented
earlier, one addition is performed for each value of the
stream besides the first two. This is because values of the
stream aren't calculated multiple times -- in other words,
because the @tt{delay} special form memoizes values. If
this were not the case, then the situation would be much
different:

@itemlist[
 @item{@tt{(stream-ref fibs 2)} would need to perform one
  addition}
 @item{@tt{(stream-ref fibs 3)} would need to perform two
  additions: One for the last value in @tt{fibs} and one
  more}
 @item{@tt{(stream-ref fibs 4)} would need to perform four
  additions: Two for the last value, one for the value
  before that, and one more}
 @item{@tt{(stream-ref fibs 5)} would need to perform seven
  additions: Four for the last value, two for the value
  before that, and one more}
 @item{@tt{(stream-ref fibs 6)} would need to perform
  twelve additions: Seven for the last value, four for the
  value before that, and one more}
 ]

The growth in the number of additions is @tt{O(2^n)}. (This
is not a tight bound, but the growth of the sequence is
clearly exponential.)

@section[#:tag "c3e58"]{Exercise 3.58}

The procedure @tt{expand} implements long division,
returning a stream of digits  in the result (after the
decimal -- it is assumed that the numerator is smaller than
the denominator). It also lets the user specify the base,
or @tt{radix}, of the result.

@section[#:tag "c3e59"]{Exercise 3.59}

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (integrate-series s)
   (define (integrate-inner strim index)
     (if (stream-null? strim)
         the-empty-stream
         (cons-stream
          (/ (stream-car strim) index)
          (integrate-inner (stream-cdr strim) (+ index 1)))))
  (integrate-inner s 1))
 ]

@examples[
 #:label #f #:eval ev #:no-prompt
 (define cosine-series
   (cons-stream 1 (integrate-series (scale-stream sine-series -1))))

 (define sine-series
   (cons-stream 0 (integrate-series cosine-series)))
 ]

@section[#:tag "c3e60"]{Exercise 3.60}

In order to multiply two series together, we need to distribute the
multiplication over addition. In other words, when multiplying
the coefficients @tt{a@subscript{0}, a@subscript{1}, ...} and
@tt{b@subscript{0}, b@subscript{1}, ...}, we need to multiply
@tt{a@subscript{0}} by every @tt{b@subscript{i}}.

The first element of this stream will naturally be
@tt{a@subscript{0} * b@subscript{0}}. The rest of the coefficients
can be found by summing two other series: One being the rest of
the coefficients @tt{b@subscript{i}} multiplied by @tt{a@subscript{0}},
and the other being the multiplication of the streams of coefficients
@tt{a@subscript{1}, ...} and every @tt{b@subscript{i}}. This can be
defined as follows:

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (mul-series s1 s2)
   (cons-stream (* (stream-car s1)
                   (stream-car s2))
                (add-streams (scale-stream (stream-cdr s2) 
                                           (stream-car s1))
                             (mul-series (stream-cdr s1) 
                                         s2))))
 ]

We can use this to verify the identity
@tt{sin@superscript{2}x + cos@superscript{2}x = 1}:

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (take-stream s n)
   (if (<= n 0)
       the-empty-stream
       (cons-stream (stream-car s)
                    (take-stream (stream-cdr s) (- n 1)))))
 
 (define sine-cosine-identity
   (add-streams (mul-series sine-series sine-series)
                (mul-series cosine-series cosine-series)))
 ]

The expected coefficients should all be zero, with a leading
constant @tt{1}. And indeed, this is the case:

@examples[
 #:label #f #:eval ev
 (display-stream (take-stream sine-cosine-identity 5))
 ]

@section[#:tag "c3e61"]{Exercise 3.61}

To find the inverse @tt{X} of the unit power series @tt{S}, we can simply translate the
equality @tt{X = S@subscript{0} - S@subscript{R}*X} (where @tt{S@subscript{0}} is the constant
term of @tt{S} and @tt{S@subscript{R}} is the coefficients of @tt{S} after the constant) into a procedure:

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (invert-unit-series s)
   (cons-stream (stream-car s)
                (scale-stream (mul-series (stream-cdr s) (invert-unit-series s)) -1)))
 ]

If we take the inverse of a power series (say, the series representing @tt{e@superscript{x}})
and multiply it by the original series, we should get a constant result of @tt{1}. We can
verify that this is the case:

@examples[
 #:label #f #:eval ev
 (define exp-series
   (cons-stream 1 (integrate-series exp-series)))
 (display-stream (take-stream (mul-series exp-series (invert-unit-series exp-series)) 5))
 ]

@section[#:tag "c3e62"]{Exercise 3.62}

Dividing a series @tt{S@subscript{1}} by series @tt{S@subscript{2}} is equivalent to multiplying
@tt{S@subscript{1}} by the inverse of @tt{S@subscript{2}}. This can be expressed succinctly with
the definitions @tt{mul-series} and @tt{invert-unit-series}:

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (div-series s1 s2)
   (if (= (stream-car s2) 0)
       (error "Cannot divide by 0")
       (mul-series s1 (invert-unit-series s2))))
 ]

We can use @tt{div-series} to define the series for @tt{tan}, which is equivalent to the series
for @tt{cosine} divided by the series for @tt{sine}:

@examples[
 #:label #f #:eval ev #:no-prompt
 (define tangent-series (div-series sine-series cosine-series))
 ]

@section[#:tag "c3e63"]{Exercise 3.63}

Every time @tt{(sqrt-stream x)} is called, a new stream is
created. This means that, if we instead define it to recursively
call itself instead of defining a local value @tt{guesses},
it cannot take advantage of the fact that values have already
been computed. Instead, an exponential number of redundant
computations will be performed. This is similar to what would
happen if @tt{delay} did not use memoization.

@section[#:tag "c3e64"]{Exercise 3.64}

To define @tt{stream-limit}, all we need to do is examine the
two most recent values of the stream that we have seen and compare
their difference to our tolerance. By now, writing this sort of
procedure is not difficult.

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (stream-limit s tolerance)
   (define (limit-inner last rest)
     (let ((r (stream-car rest)))
       (if (< (abs (- last r)) tolerance)
           r
           (limit-inner r (stream-cdr rest)))))
   (limit-inner (stream-car s) (stream-cdr s)))
]

@section[#:tag "c3e65"]{Exercise 3.65}

We can define the terms of the series equivalent
to the natural logarithm of @tt{2} as follows:

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (ln-summands n)
   (cons-stream (/ 1.0 n)
                (stream-map - (ln-summands (+ n 1)))))
 ]

We can then define an initial, non-accelerated version
of the series converting on the value @tt{ln 2} as follows:

@examples[
 #:label #f #:eval ev #:no-prompt
 (define ln-stream (partial-sums (ln-summands 1)))
 ]

Using Euler acceleration with @tt{euler-transform}, we
can define an accelerated version of this stream as follows:

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (euler-transform s)
   (let ((s0 (stream-ref s 0))
         (s1 (stream-ref s 1))
         (s2 (stream-ref s 2)))
     (cons-stream (- s2 (/ (square (- s2 s1))
                           (+ s0 (* -2 s1) s2)))
                  (euler-transform (stream-cdr s)))))
 
 (define ln-stream++ (euler-transform ln-stream))
 ]

And using tableau acceleration, we can define a "super"-accelerated
stream:

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (make-tableau transform s)
   (cons-stream s
                (make-tableau transform
                              (transform s))))

 (define (accelerated-sequence transform s)
   (stream-map stream-car
               (make-tableau transform s)))
 
 (define ln-stream# (accelerated-sequence euler-transform ln-stream))
 ]

If we want to test how quickly these streams converge on the correct
value of @tt{ln 2}, we can write a simple procedure, analogous to
@tt{stream-limit}, that will count the number of stream values that
need to be processed until two consecutive values are within a certain
tolerance of the correct value.

@examples[
 #:eval ev #:hidden
 (define (average x y )
   (/ (+ x y) 2))
 ]

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (stream-values-until-precise s correct tolerance)
   (define (count i last rest)
     (let ((r (stream-car rest)))
       (if (< (abs (- correct (average last r))) tolerance)
           i
           (count (+ i 1) r (stream-cdr rest)))))
   (count 2 (stream-car s) (stream-cdr s)))
 ]

The initial value passed to @tt{count} is @tt{2} because, in the case
that the first two values of the stream are within tolerance bounds, we
would say that @tt{2} stream values have been consumed before reaching
this precision. In other words, the minimal number of stream values that
need to be examined before the last two are within tolerance is, of course,
@tt{2}. (It is merely a convention that the value @tt{i} denotes the number
of values that will have been processed after that iteration of @tt{count}).

Noting that scheme already defines a fuction @tt{log} for
finding the natural logarithm, we can then ask how many
values it takes to reach the correct value within a small
tolerance (disclosure: which is kept somewhat large to keep
the page generation time low) as follows:

@examples[
 #:label #f #:eval ev
 (define correct (log 2))
 (define tolerance 0.0000001)
 (stream-values-until-precise ln-stream correct tolerance)
 (stream-values-until-precise ln-stream++ correct tolerance)
 (stream-values-until-precise ln-stream# correct tolerance)
 ]

@section[#:tag "c3e66"]{Exercise 3.66}

@bold{TODO}