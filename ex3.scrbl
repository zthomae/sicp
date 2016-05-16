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

@examples[#:label #f #:eval ev #:no-prompt
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

@examples[#:label #f #:eval ev #:no-prompt
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

@examples[#:label #f #:eval ev #:no-prompt
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

@examples[#:label #f #:eval ev #:no-prompt
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

@examples[#:label #f #:eval ev #:no-prompt
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

@examples[#:label #f #:eval ev #:no-prompt
(define (estimate-integral P trials x1 x2 y1 y2)
  (define (test-point)
    (let ((x (random-in-range x1 x2))
          (y (random-in-range y1 y2)))
      (P x y)))
  (monte-carlo trials test-point))
]

We can define the predicate in the book like this:

@examples[#:label #f #:eval ev #:no-prompt
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

@examples[#:label #f #:eval ev #:no-prompt
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

@examples[#:label #f #:eval ev #:no-prompt
(define (make-joint account password new-password)
  (lambda (p m)
    (if (eq? p new-password)
        (account password m)
        (lambda args "Incorrect password"))))
]

@section[#:tag "c3e8"]{Exercise 3.8}

I feel like I've come up with an ugly solution. I'm going to mark this as
@bold{TODO} for now.

@examples[#:label #f #:eval ev #:no-prompt
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

@examples[#:label #f #:eval ev #:no-prompt
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

@examples[#:label #f #:eval ev #:no-prompt
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

@examples[#:label #f #:eval ev #:no-prompt
(define (make-cycle x)
  (set-cdr! (last-pair x) x))
]

Then, whe we try to compute @tt{(last-pair x)}, we get an infinite loop,
because the end of @tt{x} now points to the front of @tt{x}. In other words,
we will never come across a pair where the @tt{cdr} is @tt{nil}, so
@tt{last-pair} will never terminate.

@section[#:tag "c3e14"]{Exercise 3.14}

Consider the procedure below:

@examples[#:label #f #:eval ev #:no-prompt
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

@examples[#:label #f #:eval ev #:no-prompt
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

@examples[#:label #f #:eval ev #:no-prompt
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

@examples[#:label #f #:eval ev #:no-prompt
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

@examples[#:label #f #:eval ev #:no-prompt
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

@examples[#:label #f #:eval ev #:no-prompt
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

@examples[#:label #f #:eval ev #:no-prompt
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

@examples[#:label #f #:eval ev #:no-prompt
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

@examples[#:label #f #:eval ev #:no-prompt
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

@examples[#:label #f #:eval ev #:no-prompt
(define (lookup table)
  (lambda (keys)
    (if (null? keys) table
        (let ((record (assoc (car keys) (cdr table))))
          (if record
              ((lookup record) (cdr keys))
              false)))))
]

@examples[#:label #f #:eval ev #:no-prompt
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

In order to store any orderable data type as a key in the
table, we must also supply an ordering function to these
operations. There are two options:

@itemlist[
 @item["Make the ordering function an argument to the table functions"]
 @item["Store the ordering function in the table"]
]