#lang scribble/manual

@require[scribble/lp]

@title[#:version "" #:style 'toc]{Chapter 2}

@local-table-of-contents[]

@section[#:tag "c2e1"]{Exercise 2.1}

This new @tt{make-rat} procedure is ultimately a case analysis. There are
many ways to do it -- I have chosen one that handles both cases of negative
rational numbers in one case.

@codeblock{
(define (make-rat n d)
  (cond ((and (> n 0) (> d 0))
         (cons n d))
        ((and (< n 0) (< d 0))
         (cons (abs n) (abs d)))
        (else
         (cons (* -1 (abs n)) (abs d)))))
}

@section[#:tag "c2e2"]{Exercise 2.2}

The procedures for making and selecting from line segments and points are very similar --
just operations on pairs of their parts.  @tt{make-segment} and @tt{make-point} are both calls
to @tt{cons}, @tt{start-segment} and @tt{x-point} are both @tt{car}s, and @tt{end-segment} and
@tt{y-point} are both @tt{cdr}s.

@codeblock{
(define (make-segment start end)
  (cons start end))
}

@codeblock{
(define (start-segment segment)
  (car segment))
}

@codeblock{
(define (end-segment segment)
  (cdr segment))
}

@codeblock{
(define (make-point x y)
  (cons x y))
}

@codeblock{
(define (x-point point)
  (car point))
}

@codeblock{
(define (y-point point)
  (cdr point))
}

@tt{midpoint-segment} is a simple operation relying on an @tt{average} procedure that
is trivial enough to not include here.

@codeblock{
(define (midpoint-segment s)
  (let ((x (average (x-point (start-segment s))
                    (x-point (end-segment s))))
        (y (average (y-point (start-segment s))
                    (y-point (end-segment s)))))
    (make-point x y)))
}

To illustrate the power of these similar means of abstraction even more, we can
generalize the @tt{print-point} procedure. The original as listed in the exercise
is as follows:

@codeblock{
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))
}

We could implement a separate @tt{print-segment} procedure that calls @tt{print-point},
but I believe we can do better. @tt{print-point} can be imagined as an instance
of a process doing the following things:

@itemlist[
@item{Print the pre-matter (in this case, a newline and "(")}
@item{Print the @tt{car} of the pair}
@item{Print the between-matter (",")}
@item{Print the @tt{cdr} of the pair}
@item{Print the end-matter (")")}
]

Therefore, we could write a generalized @tt{print-pair} procedure and implement
@tt{print-point}, and also a new procedure @tt{print-segment}, with it.

@codeblock{
(define (print-pair pair pre between end inner-print)
  (display pre)
  (display (car pair))
  (display between)
  (display (cdr pair))
  (display end))
}

However, while this will work perfectly well for reimplementing @tt{print-point},
it works less than well for making @tt{print-segment}. It would be desirable to
have @tt{print-segment} call @tt{print-point} on each of its points, but @tt{print-pair}
will necessarily call @tt{display} on them, printing them with the environment's
default representation for pairs. This will give a readable printout, but I believe
we should do better. This can be done by supplying one more argument to print-pair,
an @tt{inner-print} procedure that is called to print the @tt{car} and @tt{cdr} of the pair
And so we get the actual @tt{print-pair} procedure:

@codeblock{
(define (print-pair pair pre between end inner-print)
  (display pre)
  (inner-print (car pair))
  (display between)
  (inner-print (cdr pair))
  (display end))
}

And with this, we can make @tt{print-point} and @tt{print-segment}:

@codeblock{
(define (print-point p)
  (print-pair p "(" "," ")" display))
}

@codeblock{
(define (print-segment s)
  (print-pair s "" " -> " "" print-point))
}

There is another benefit that we can get out of this. Suppose we implemented
@tt{print-segment} using the first version of @tt{print-point} given in the book.
Each point would have a newline before it, necessarily and always. If we wanted
to change this (say, to print a segment on one line), we would have to write a
new @tt{print-point} procedure. This is significantly easier to handle with the new
@tt{print-pair} procedure -- we can simply supply newlines to be printed wherever
we want. The problem could be solved like so:

@codeblock{
(define (println-segment s)
  (print-pair s "" " -> " "\n" print-point))
}

It would be even better if the @tt{car}s and @tt{cdr}s of the pairs sent to
@tt{print-pair} knew how to print themselves, but that is getting beyond the scope
of the exercise.

@section[#:tag "c2e3"]{Exercise 2.3}

Two representations of rectangles come to mind immediately:

@itemlist[
@item{A pair of width and height segments from a corner of the rectangle}
@item{A pair of points: @nested{@itemlist[
  @item{A bottom corner from the origin}
  @item{A top corner from the origin}
]}}]

To make sure that we produce rectangles, the connections to the origin will
need to be tested in both of the construction procedures.

First, the procedures for making rectangles out of width and height line segments:

@codeblock{
(define (make-rectangle-segments width height)
  (define (is-origin point)
    (= 0 (x-point point) (y-point point)))
  (let ((origin-width (start-segment width))
        (origin-height (start-segment height)))
    (cond ((not (is-origin origin-width))
           (error "width segment must be from origin"))
          ((not (is-origin origin-height))
           (error "height segment must be from origin"))
          ((not (= 0 (y-point (end-segment width))))
           (error "width segment must have no y component"))
          ((not (= 0 (x-point (end-segment height))))
           (error "height segment must have no x component"))
          (else (cons width height)))))
}

This procedure is, after error-checking to make sure both the width and height
segments begin from the origin and that they are both parallel to their
respective axes, just a @tt{cons} call like the others.

The segment selectors are just as simple as the others. But in order to
calculate the area and perimeter of the rectangle, we need to get the
width and height of the rectangle as numbers. By extracting the coordinates
from the ends of the width and height segments, we can do this:

@codeblock{
(define (get-width-segment rect) (car rect))

(define (get-width rect)
  (x-point (end-segment (get-width-segment rect))))
}

@codeblock{
(define (get-height-segment rect) (cdr rect))

(define (get-height rect)
  (y-point (end-segment (get-height-segment rect))))
}

Then we can calculate the area and perimeter using these @tt{get-width} and
@tt{get-height} procedures:

@codeblock{
(define (area rect)
  (* (get-width rect) (get-height rect)))
}

@codeblock{
(define (perimeter rect)
  (* 2 (+ (get-width rect) (get-height rect))))
}

Now let's move on to our second representation, using a bottom ("width") corner
and a top ("height") corner -- or perhaps it's better to describe this as being
made of the @tt{end-segment} points from the width and height segments used in
the last representation. Since it takes less complex data in its construction,
the new procedure @tt{make-rectangle-points} is simpler in providing the same
guarantees as @tt{make-rectangle-segments}, because it has no origins points in
line segments to check.

@codeblock{
(define (make-rectangle-points width-corner height-corner)
  (cond ((not (= 0 (y-point width-corner)))
         (error ("width corner must have no y component")))
        ((not (= 0 (x-point height-corner)))
         (error ("height corner must have no x component")))
        (else (cons width-corner height-corner))))
}

Getting the width and height of the rectangle is also easier, since there
are fewer data structures to traverse:

@codeblock{
(define (get-width-corner rect)
  (car rect))

(define (get-width rect)
  (x-point (get-width-corner rect)))
}

@codeblock{
(define (get-height-corner rect)
  (cdr rect))

(define (get-height rect)
  (y-point (get-height-corner rect)))
}

And with that, we can use @tt{area} and @tt{perimeter} just as before.

In a real implementation, it would be better to hide references to the
implementation-specific details, like @tt{-segment} and @tt{-corner}, behind
standard interfaces like @tt{make-rectangle}, @tt{get-width}, and
@tt{get-height}. More on this topic will be introduced later in the book.

@section[#:tag "c2e4"]{Exercise 2.4}

The definitions of @tt{cons} and @tt{car} we are given are as follows:

@codeblock{
(define (cons x y)
  (lambda (m) (m x y)))
}

@codeblock{
(define (car z)
  (z (lambda (p q) p)))
}

@tt{cons} is a procedure of two arguments, @tt{x} and @tt{y}, that returns a
procedure of one argument, @tt{m}, that calls @tt{m} on @tt{x} and @tt{y}.
@tt{car} is a procedure taking a procedure @tt{z} as its only argument, and it
calls @tt{z} on a procedure that returns the first of two arguments given to it.

We can see how this evaluates by substituting:

@verbatim{
(car z)
(z (lambda (p q) p))
((lambda (m) (m x y)) (lambda (p q) p))
((lambda (p q) p) x y)
x
}

We know that @tt{(car z)} should return @tt{x} by definition, so we can conclude
that @tt{car} is correct.

We can do similarly for @tt{cdr}. Its definition is almost identical to @tt{car}'s,
except it passes a procedure returning its second of two arguments.

@codeblock{
(define (cdr z)
  (z (lambda (p q) q)))
}

Using the substitution model to evaluate:

@verbatim{
(cdr z)
(z (lambda (p q) q))
((lambda (m) (m x y)) (lambda (p q) q))
((lambda (p q) q) x y)
y
}

@section[#:tag "c2e5"]{Exercise 2.5}

@bold{TODO}

@section[#:tag "c2e6"]{Exercise 2.6}

@bold{TODO: Words}

@codeblock{
(define zero (lambda (f) (lambda (x) x)))
}

@codeblock{
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
}

@verbatim{
(n f)
((lambda (f) (lambda (x) x)) f)
(lambda (x) x)

((n f) x)
((lambda (x) x) x)
x

(f ((n f) x))
(f x)

(lambda (f) (lambda (x) (f ((n f) x))))
(lambda (f) (lambda (x) (f x)))
}

@codeblock{
(define one (lambda (f) (lambda (x) (f x))))
}

@verbatim{
(n f)
((lambda (f) (lambda (x) (f x))) f)
(lambda (x) (f x))

((n f) x)
((lambda (x) (f x)) x)
(f x)

(f ((n f) x))
(f (f x))

(lambda (f) (lambda (x) (f ((n f) x))))
(lambda (f) (lambda (x) (f (f x))))
}

@codeblock{
(define two (lambda (f) (lambda (x) (f (f x)))))
}

@codeblock{
(define (add-church m n)
  (lambda (f)
    (lambda (x)
      ((m f) ((n f) x)))))
}

@section[#:tag "c2e7"]{Exercise 2.7}

We can see by examining the implementations of the operations that @tt{make-interval}
takes the lower bound as the first parameter and the upper bound as the second. For
example, in @tt{add-interval}:

@codeblock{
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
}

Therefore, knowing that @tt{make-interval} is a @tt{cons} call, we know to define
@tt{lower-bound} and @tt{upper-bound} as so:

@codeblock{
(define (lower-bound i) (car i))
}

@codeblock{
(define (upper-bound i) (cdr i))
}

@section[#:tag "c2e8"]{Exercise 2.8}

Alyssa reasoned that adding two intervals made a new interval where the lower
bound was the sum of the arguments' lower bounds and the upper bound was the
sum of the arguments' upper bounds. We can define a similar procedure
@tt{sub-interval} by supposing that subtracting two intervals creates an interval
where the lower bound is the difference between the two lower bounds (and similarly
for the upper bound).

@codeblock{
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (lower-bound x) (lower-bound y))))
}

@section[#:tag "c2e9"]{Exercise 2.9}

First, a definition of the @tt{width} procedure:

@codeblock{
(define (width x) (/ (- (upper-bound x) (lower-bound x)) 2))
}

We can use algebra to define the widths of the sum and difference of intervals
in terms of the widths of the operands:

@verbatim{
Let x and y be intervals (ax, bx) and (ay, by)
width(x) = (ax + bx) / 2
width(y) = (ay + by) / 2

width(x+y) = ((ax + ay) + (bx + by)) / 2
           = ((ax + bx) + (ay + by)) / 2
           = width(x) + width(y)

Similarly,
width(x - y) = ((ax - ay) + (bx - by)) / 2
             = ((ax + bx) - (ay + by)) / 2
             = width(x) - width(y)
}

To show that the width of a multiplied (or divided) pair of intervals is not a
function of the widths of these intervals, we can construct two intervals with
the same width and compare the widths of these intervals multiplied (or
divided) by a third interval.

I could demonstrate this algebraically, but instead I will operate on the intervals
using our procedures:

@verbatim{
(define i1 (make-interval 0 5))
(define i2 (make-interval 5 10))
(= (width i1) (width i2))
=> #t

(define i3 (make-interval 1 10))

(= (width (mul-interval i1 i3)) (width (mul-interval i2 i3)))
=> #f

(= (width (div-interval i1 i3)) (width (div-interval i2 i3)))
=> #f
}

@section[#:tag "c2e10"]{Exercise 2.10}

Before modifying any code, we need to understand: Why does it not make sense
to divide by an interval that spans zero?

When in doubt, we can manipulate interval with our procedures to investigate.
But first, a procedure for computing the reciprocal of an interval:

@codeblock{
(define (reciprocal i)
  (make-interval (/ 1.0 (upper-bound i))
                 (/ 1.0 (lower-bound i))))
}

Now we can do the following:

@verbatim{
(define i1 (make-interval 1 10))
(define i2 (make-interval -5 5))

(define ri1 (reciprocal i1))
(define ri2 (reciprocal i2))

ri1
=> (0.1 . 1.0)

ri2
=> (0.2 . -0.2)
}

Notice how the upper bound of @tt{ri1} is still greater than the lower bound, but that
this is @emph{not} true for @tt{ri2}. In other words, taking the reciprocal of an interval
spanning zero did not create a valid interval.

We can explore this more algebraically. Let @tt{i} be an interval not spanning
@tt{0}, and let @tt{x} be its lower bound and @tt{y} be its upper bound.  By
definition, @tt{x < y}. It is trivial to see that @tt{(1 / x) > (1 / y)}, and
that an interval defined by @tt{((1 / y), (1 / x))} is valid.

However, suppose @tt{i} does span zero, such that @tt{x < 0 < y}. It is then
true that @tt{(1 / x) < 0} and @tt{(1 / y) > 0} -- in other words, that
@tt{(1 / x) < (1 / y)}. An interval defined by @tt{((1 / y), (1 / x))} is
therefore not valid.

@bold{TODO: I think I'm on to something, but multiplying will still make an interval}

Fixing Alyssa's code is simple (and more readable because of the new
@tt{reciprocal} procedure):

@codeblock{
(define (div-interval x y)
  (if (and (< (lower-bound y) 0) (> (upper-bound y) 0))
      (error "Cannot divide by an interval spanning zero")
      (mul-interval x (reciprocal y))))
}

@section[#:tag "c2e11"]{Exercise 2.11}

Ben's point is that there are nine different valid sign combinations for the
lower and upper bounds of the two intervals being multiplied, and that a basic
application of inequalities can will show that the bounds of the new interval
are, in all cases except one, knowable directly, without having to get the
@tt{min} or @tt{max} of a group of possible bounds.  Making the new
@tt{mul-interval} procedure is not hard, but it is tedious and easy to make
mistakes. Therefore, we will make a basic testing procedure to make sure that
our new procedure agrees with the existing @tt{mul-interval} for all sign combinations.

First, a useful procedure that should already have been defined, for determining if
two intervals are the same:

@codeblock{
(define (equal-intervals? x y)
  (and
   (= (lower-bound x) (lower-bound y))
   (= (upper-bound x) (upper-bound y))))
}

We can use this to test if multiplied intervals by two different procedures are
equal. Applying this to every combination of signs for the bounds of two intervals
(whose bounds are chosen arbitrarily, and should not be important), we can make a
procedure to exhaustively test the nine sign combination cases to make sure that
our new procedure gets the same result as the old one every time.

@codeblock{
(define (test-new-mul-interval new-mul-interval)
  (define (equal-products? x y)
    (equal-intervals? (mul-interval x y) (new-mul-interval x y)))
  (define (neg x) (* x -1))
  (let ((lx 1)
        (ux 10)
        (ly 2)
        (uy 4))
    (cond
     ((not (equal-products?
            (make-interval lx ux) (make-interval ly uy)))
      (error "new-mul-interval fails for lx > 0, ux > 0, ly > 0, uy > 0"))
     ((not (equal-products?
            (make-interval lx ux) (make-interval (neg ly) uy)))
      (error "new-mul-interval fails for lx > 0, ux > 0, ly < 0, uy > 0"))
     ((not (equal-products?
            (make-interval lx ux) (make-interval (neg ly) (neg uy))))
      (error "new-mul-interval fails for lx > 0, ux > 0, ly < 0, uy < 0"))
     ((not (equal-products?
            (make-interval (neg lx) ux) (make-interval ly uy)))
      (error "new-mul-interval fails for lx < 0, ux > 0, ly > 0, uy > 0"))
     ((not (equal-products?
            (make-interval (neg lx) ux) (make-interval (neg ly) uy)))
      (error "new-mul-interval fails for lx < 0, ux > 0, ly < 0, uy > 0"))
     ((not (equal-products?
            (make-interval (neg lx) ux) (make-interval (neg ly) (neg uy))))
      (error "new-mul-interval fails for lx < 0, ux > 0, ly < 0, uy < 0"))
     ((not (equal-products?
            (make-interval (neg lx) (neg ux)) (make-interval ly uy)))
      (error "new-mul-interval fails for lx < 0, ux < 0, ly > 0, uy > 0"))
     ((not (equal-products?
            (make-interval (neg lx) (neg ux)) (make-interval (neg ly) uy)))
      (error "new-mul-interval fails for lx < 0, ux < 0, ly < 0, uy > 0"))
     ((not (equal-products?
            (make-interval (neg lx) (neg ux)) (make-interval (neg ly) (neg uy))))
      (error "new-mul-interval fails for lx < 0, ux < 0, ly < 0, uy < 0"))
     (else #t))))
}

The code is not pretty, but it works, and will pinpoint exactly what cases are
in error. I made mistakes when I first wrote the new multiplication procedure,
and this was helpful.

Below is the new multiplication procedure using a reduced number of multiplications.
It uses a @tt{cond} statement checking combinations of signs for all bounds (and as
a stylistic choice, leaves the case with more than @tt{2} multiplications for the
@tt{else} case).

@codeblock{
(define (mul-interval-ben x y)
  (let ((lx (lower-bound x))
        (ux (upper-bound x))
        (ly (lower-bound y))
        (uy (upper-bound y)))
    (cond
     ((and (> lx 0) (> ux 0) (> ly 0) (> uy 0))
      (make-interval (* lx ly) (* ux uy)))
     ((and (> lx 0) (> ux 0) (< ly 0) (> uy 0))
      (make-interval (* ux ly) (* ux uy)))
     ((and (> lx 0) (> ux 0) (< ly 0) (< uy 0))
      (make-interval (* ux uy) (* lx ly)))
     ((and (< lx 0) (> ux 0) (> ly 0) (> uy 0))
      (make-interval (* lx uy) (* ux uy)))
     ((and (< lx 0) (> ux 0) (< ly 0) (< uy 0))
      (make-interval (* ux uy) (* lx uy)))
     ((and (< lx 0) (< ux 0) (> ly 0) (> uy 0))
      (make-interval (* ux uy) (* lx ly)))
     ((and (< lx 0) (< ux 0) (< ly 0) (> uy 0))
      (make-interval (* ux uy) (* ux ly)))
     ((and (< lx 0) (< ux 0) (< ly 0) (< uy 0))
      (make-interval (* lx ly) (* ux uy)))
     (else
      (make-interval (min (* lx uy) (* ux ly))
                     (max (* lx ly) (* ux uy)))))))
}

And using our testing procedure, we can verify that this procedure is correct:

@verbatim{
(test-new-mul-interval mul-interval-ben)
=> #t
}

@section[#:tag "c2e12"]{Exercise 2.12}

@tt{make-center-percent} is not hard to implement. Just calculate the width
from the center and the percent and then use @tt{make-center-width}:

@codeblock{
(define (make-center-percent c p)
  (let ((w (* c (/ p 100))))
    (make-center-width c w)))
}

@tt{percent} can be calculated by subtracting the center from the upper bound
and then dividing by the width:

@codeblock{
(define (percent i)
  (let ((c (center i))
        (w (width i)))
    (/ (- (upper-bound i) c) w)))
}

@section[#:tag "c2e13"]{Exercise 2.13}

@bold{TODO}

@section[#:tag "c2e14"]{Exercise 2.14}

@bold{TODO}

@section[#:tag "c2e15"]{Exercise 2.15}

@bold{TODO}

@section[#:tag "c2e16"]{Exercise 2.16}

@bold{TODO}

@section[#:tag "c2e17"]{Exercise 2.17}

@bold{TODO: Words}

@codeblock{
(define (last-pair l)
  (if (null? (cdr l)) l
      (last-pair (cdr l))))
}

@section[#:tag "c2e18"]{Exercise 2.18}

@bold{TODO: Words}

@codeblock{
(define (reverse l)
  (if (null? (cdr l)) l
      (append (reverse (cdr l)) (list (car l)))))
}

@section[#:tag "c2e19"]{Exercise 2.19}

@tt{first-denomination}, @tt{except-first-denomination}, and @tt{no-more?} are all primitive
operations on lists:

@codeblock{
(define (first-denomination coin-values)
  (car coin-values))
}

@codeblock{
(define (except-first-denomination coin-values)
  (cdr coin-values))
}

@codeblock{
(define (no-more? coin-values)
  (null? coin-values))
}

The order of the list @tt{coin-values} does not affect the answer of @tt{cc}:

@verbatim{
(= (cc 100 us-coins) (cc 100 (reverse us-coins)))
=> #t
}

@section[#:tag "c2e20"]{Exercise 2.20}

I found it easiest to define @tt{same-parity} iteratively, using an iterative inner
procedure taking the first argument, the results list, and the rest of the numbers
to check as arguments.

@codeblock{
(define (same-parity x . xs)
  (define (is-same-parity? x y)
    (= (remainder x 2) (remainder y 2)))
  (define (same-parity-iter x l xs)
    (cond ((null? xs) l)
          ((is-same-parity? x (car xs))
           (same-parity-iter x (append l (list (car xs))) (cdr xs)))
          (else
           (same-parity-iter x l (cdr xs)))))
  (same-parity-iter x nil xs))
}

One of the reasons I chose to do it this way is that @tt{same-parity} expects
its arguments to be individual, rather than in the form of a list. I could
alternatively have used @tt{apply} to get around this:

@codeblock{
(define (same-parity x . xs)
  (define (is-same-parity? x y)
    (= (remainder x 2) (remainder y 2)))
  (cond ((null? xs) nil)
        ((is-same-parity? x (car xs))
         (cons (car xs) (apply same-parity (cons x (cdr xs)))))
        (else
         (apply same-parity (cons x (cdr xs))))))
}

However, although I have used it before, we technically don't know @tt{apply}
at this point in the book.

@section[#:tag "c2e21"]{Exercise 2.21}

First, the implementation of @tt{square-list} that does not use @tt{map}:

@codeblock{
(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items))
            (square-list (cdr items)))))
}

And the implementation that does:

@codeblock{
(define (square-list items)
  (map (lambda (x) (square x)) items))
}

@section[#:tag "c2e22"]{Exercise 2.22}

The first procedure produces the answer list in the wrong order because it
@tt{cons}es the square of the current entry to the front of the @tt{answers}
list. You can do this and get the result list in the correct order when in a
recursive process because the @tt{cons} calls will be evaluated in reverse
order, appending earlier items in the list to the front. However, when in an
iterative process, you append to the front of the list in order of traversal,
meaning that later items are squared and appear earlier in the list.

The second procedure does not work because @tt{cons}es a list to a value,
which does not produce a valid list structure.

@section[#:tag "c2e23"]{Exercise 2.23}

I have chosen to use @tt{nil} as the return value rather than true. This isn't
exactly what I'd like to do, but I don't think @tt{for-each} should have a
return value, and at this time I think this is closer to that.

@codeblock{
(define (for-each f items)
  (if (null? items) nil
      ((lambda ()
         (f (car items))
         (for-each f (cdr items))))))
}

@section[#:tag "c2e24"]{Exercise 2.24}

Omitted.

@section[#:tag "c2e25"]{Exercise 2.25}

I give solutions both in reduced and unreduced form.

@verbatim{
(define 25-1 (list 1 3 (list 5 7) 9))
(= 7 (car (cdr (car (cdr (cdr 25-1))))))
=> #t
(= 7 (car (cdaddr 25-1)))
=> #t

(define 25-2 (list (list 7)))
(= 7 (car (car 25-2)))
=> #t
(= 7 (caar 25-2))
=> #t

(define 25-3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(= 7 (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr 25-3)))))))))))))
=> #t
(= 7 (cadadr (cadadr (cadadr 25-3))))
=> #t
}

@section[#:tag "c2e26"]{Exercise 2.26}

@verbatim{
(append x y)
=> (1 2 3 4 5 6)
}

We get this result because @tt{append} takes two lists and returns a new list
containing all the items from the first list followed by all the items from
the second list.

@verbatim{
(cons x y)
=> ((1 2 3) 4 5 6)
}

We get this result because @tt{cons} creates a pair where the first element points
to the list @tt{(1 2 3)} and is  is the valid list @tt{(4 5 6)}.

@verbatim{
(list x y)
=> ((1 2 3) (4 5 6))
}

We get this result because @tt{list} creates a list structure where the first element
is the list @tt{(1 2 3)} and the second element is the list @tt{(4 5 6)}.

@section[#:tag "c2e27"]{Exercise 2.27}

There are a few subtle changes in @tt{deep-reverse} that should be mentioned.

First, since all list structures need to be reversed, the base case now returns
an atom and not a list. This means that instead of calling @tt{append}, the
general case needs to call @tt{cons}.

Second, although the first base case is for whether the argument is not a pair,
the case checking if the argument is a single-value list is still needed to
produce correct list structures. The one item list case returns
@tt{deep-reverse (car l)}, while the general case returns
@tt{(cons (deep-reverse (cdr l)) (list (deep-reverse (car l))))} (where the second argument
is still turned into a list).

The procedure is as follows:

@codeblock{
(define (deep-reverse l)
  (cond
   ((not (pair? l)) l)
   ((null? (cdr l)) (deep-reverse (car l)))
   (else (cons (deep-reverse (cdr l)) (list (deep-reverse (car l)))))))
}

There need to be three cases here because even though @tt{(deep-reverse (cdr l))} will
return @tt{nil} if that's what @tt{(cdr l)} is, @tt{cons}ing @tt{nil} in front will
create the wrong list structure. In other words,

@verbatim{
(define (deep-reverse l)
  (cond
   ((not (pair? l)) l)
  ;((null? (cdr l)) (deep-reverse (car l)))
   (else (cons (deep-reverse (cdr l)) (list (deep-reverse (car l)))))))

(deep-reverse x)
=> ((() ((() 4) 3)) ((() 2) 1))
}

@section[#:tag "c2e28"]{Exercise 2.28}

@tt{fringe} is similar to @tt{deep-reverse} in that it has three base cases for
non-lists, single-item lists, and general lists. However, the non-pair case
produces a list, and the general case uses @tt{append} to keep the resulting
list flat. Avoiding @tt{append}ing @tt{nil} to a list is still done, of course.

@codeblock{
(define (fringe l)
  (cond ((not (pair? l)) (list l))
        ((null? (cdr l)) (fringe (car l)))
        (else (append (fringe (car l)) (fringe (cdr l))))))
}

@section[#:tag "c2e29"]{Exercise 2.29}

@bold{TODO: Words}

@codeblock{
(define (left-branch mobile)
  (car mobile))
}

@codeblock{
(define (right-branch mobile)
  (cadr mobile))
}

@codeblock{
(define (branch-length branch)
  (car branch))
}

@codeblock{
(define (branch-structure branch)
  (cadr branch))
}

@codeblock{
(define (weigh-branch branch)
    (let ((s (branch-structure branch)))
      (if (not (list? s)) s
          (total-weight s))))

(define (total-weight mobile)
  (+ (weigh-branch (left-branch mobile)) (weigh-branch (right-branch mobile))))
}

@codeblock{
(define (is-mobile-balanced? mobile)
  (define (torque branch)
    (* (branch-length branch) (weigh-branch branch)))
  (define (are-submobiles-balanced? branch)
    (let ((s (branch-structure branch)))
      ;; if s is not a mobile, then all submobiles are balanced
      (if (not (list? s)) #t
          (is-mobile-balanced? s))))
  (let ((left (left-branch mobile))
        (right (right-branch mobile)))
    (if (not (= (torque left) (torque right))) #f
        (and
         (are-submobiles-balanced? left)
         (are-submobiles-balanced? right)))))
}

@bold{TODO: Testing}

@bold{TODO: Changing list to cons (just change list? to pair?, I think)}

@section[#:tag "c2e30"]{Exercise 2.30}

These procedures are direct modifications of the corresponding ones for
@tt{scale-tree}. This is relevant to the next exercise.

@codeblock{
(define (square-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))
}

@codeblock{
(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (square sub-tree)))
       tree))
}

@section[#:tag "c2e31"]{Exercise 2.31}

@codeblock{
(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map proc sub-tree)
             (proc sub-tree)))
       tree))
}

As an example, we could define @tt{scale-tree} this way:

@verbatim{
(define (scale-tree tree factor)
  (tree-map (lambda (x) (* x factor)) tree))
}

@section[#:tag "c2e32"]{Exercise 2.32}

The idea behind this algorithm is as follows: The set of all subsets of a set
@tt{s} is the same as the union of the set of subsets of @tt{s} containing an
arbitrary element @tt{x} from @tt{s} and the set of subsets of @tt{s} not
containing @tt{x}. The latter is equivalent to the set of subsets of @tt{s}
minus the set containing @tt{x}.

There is a bijection between these two subsets of the subsets of @tt{s}: To
go from a subset of @tt{s} containing @tt{x} to one not containing @tt{x}
(or vice versa), remove @tt{x} from (or add it to) it. In other words, if we
know the subsets of @tt{s} not containing @tt{x}, we can get all of the subsets
of @tt{s} by adding to this set each of its constituent subsets with @tt{x} added
to them.

In the code below, the base case is trying to get the subsets of an empty set,
which is just a set containing the empty set (@tt{(list nil)}). The element @tt{x} is
selected as the first element of the set, and so we then calculate the subsets
of @tt{(cdr s)} in order to add @tt{(car s)} to each of them and append them to the
subsets of @tt{(cdr s)} to get our answer. The operation of adding @tt{(car s)} to
a subset of @tt{(cdr s)} is:

@verbatim{
(lambda (subset)
  (append subset (list (car s))))
}

The complete procedure is below.

@codeblock{
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append
         rest
         (map (lambda (subset) (append subset (list (car s))))
              rest)))))
}

@section[#:tag "c2e33"]{Exercise 2.33}

To implement @tt{map} using @tt{accumulate}, we give @tt{accumulate} a
procedure @tt{cons}ing the result of applying @tt{p} to its first argument
(the new value) onto its second argument (the accumulated list so far).
And since @tt{accumulate} evolves a recursive process, the values of the
new sequence will be in the right order.

@codeblock{
(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))
}

To implement @tt{append}, we set up one sequence as the initial value and
apply @tt{cons} to successive elements from the other. The second argument
is used as the initial value because @tt{cons} will append new values to
the front of the list.
@codeblock{
(define (append seq1 seq2)
  (accumulate cons seq2 seq1))
}

To implement @tt{length}, we give a procedure that adds 1 to its second argument.

@codeblock{
(define (length sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))
}

@section[#:tag "c2e34"]{Exercise 2.34}

In every step, we add @tt{this-coeff} to the product of the already-computed
@tt{higher-terms} and @tt{x}.

@bold{TODO: More explanation}

@codeblock{
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* higher-terms x)))
              0
              coefficient-sequence))
}

@section[#:tag "c2e35"]{Exercise 2.35}

I can think of a silly way to write @tt{count-leaves} using the template provided
by the exercise:

@codeblock{
(define (count-leaves t)
  (accumulate (lambda (x y) (+ y 1))
              0
              (map identity (enumerate-tree t))))
}

Of course, this could be simplified like this:

@codeblock{
(define (count-leaves t)
  (accumulate (lambda (x y) (+ y 1)) 0 (enumerate-tree t)))
}

However, we don't have to write @tt{count-leaves} as an accumulation, because
we already have an @tt{accumulate} procedure for counting things: @tt{length}
from @secref{c2e33}:

@codeblock{
(define (count-leaves t)
  (length (enumerate-tree t)))
}

@section[#:tag "c2e36"]{Exercise 2.36}

The added parts of the procedure are both based on a simple idea: To get the
first element of each of @tt{seqs}, you can @tt{map} @tt{car} over them. And
to get the rest of each of them, you can @tt{map} @tt{cdr} over them.

@codeblock{
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))
}

@section[#:tag "c2e37"]{Exercise 2.37}

@tt{matrix-*-matrix} maps over each row of the matrix a procedure that maps
multiplication by the scalar @tt{v} over the entries in the row.

@codeblock{
(define (matrix-*-vector m v)
  (map (lambda (r) (map (lambda (e) (* e v)) r)) m))
}

@tt{transpose} uses @tt{cons} with @tt{accumulate-n} to create a list of lists
made up of the columns of the original matrix.

@codeblock{
(define (transpose mat)
  (accumulate-n cons nil mat))
}

@tt{matrix-*-matrix} computes the @tt{dot-product} of each row of @tt{m} with
each column of @tt{n} (where the columns are found with @tt{transpose}).

@codeblock{
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map
     (lambda (row)
       (map (lambda (col) (dot-product row col))
            cols))
     m)))
}

@section[#:tag "c2e38"]{Exercise 2.38}

@verbatim{
(fold-right / 1 (list 1 2 3))
=> 3/2
}

First @tt{(/ 3 2)} is computed, then this is divided by @tt{1}, making
@tt{3/2} the final answer.

@verbatim{
(fold-left / 1 (list 1 2 3))
=> 1/6
}

However, here @tt{1} is divided by @tt{2} first, and then this is divided
by @tt{3}, making @tt{1/6} the final answer.

@verbatim{
(fold-right list nil (list 1 2 3))
=> (1 (2 (3 ())))
}

First, @tt{3} is @tt{cons}ed with @tt{nil}. Then, @tt{2} is @tt{cons}ed with this.
Then, @tt{1} is @tt{cons}ed with this. Because a number is always @tt{cons}ed with
a list, this produces a list structure.

@verbatim{
(fold-left list nil (list 1 2 3))
(((() 1) 2) 3)
}

First, @tt{nil} is @tt{cons}ed with @tt{1}. Then, this is @tt{cons}ed with @tt{2}.
Then, this is @tt{cons}ed with @tt{3}. Because a pair is always @tt{cons}ed with
a number, this produces the opposite of a list structure.

@tt{fold-left} and @tt{fold-right} will produce the same result if @tt{op} is commutative.

@section[#:tag "c2e39"]{Exercise 2.39}

Writing @tt{reverse} in terms of @tt{fold-right} uses an inner procedure of

@codeblock{
(lambda (x y) (append y (list x)))
}

@tt{y} contains the list so far (with an initial value of @tt{nil}), while
@tt{x} is the current element being folded over. The current element gets added
to the end of the list, and since the values in the original list get added
in reverse order (due to the way the recursive process unfolds), this produces
a reversed list.

@codeblock{
(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))
}

Writing @tt{reverse} in terms of @tt{fold-left} is almost identical, with an
inner procedure of

@codeblock{
(lambda (x y) (append (list y) x))
}

The two arguments of a procedure passed to @tt{fold-left} are reversed compared to
those for @tt{fold-right}. So now, @tt{y} is the current element being visited.
Since @tt{fold-left} evolves an iterative process where the elements of the list
being reversed are operated on in forward order, appending the values to the front
of the list as they are seen produces a list in reversed order.

@codeblock{
(define (reverse-foldl sequence)
  (fold-left (lambda (x y) (append (list y) x)) nil sequence))
}

@section[#:tag "c2e40"]{Exercise 2.40}

This procedure is almost identical to the one embedded in the original
@tt{prime-sum-pairs} procedure, with one small change: The outer map (over
values of @tt{i}) now enumerates from @tt{2} to @tt{n}, not from @tt{1}.  The
inner @tt{enumerate-interval} for @tt{j} would produce an empty list (since it
would go from @tt{1} to @tt{0}), which would get ignored when @tt{flatmap}
@tt{append}s the results together (@tt{(append nil nil)} is still @tt{nil}). A
clever trick knowing how @tt{flatmap} works, but I prefer it this way. The
definition of the unique pairs given in the book states that @tt{i > 1} anyway.

@codeblock{
(define (unique-pairs n)
  (map
   (lambda (i)
     (map (lambda (j) (list i j))
          (enumerate-interval 1 (- i 1))))
   (enumerate-interval 2 n)))
}

@tt{prime-sum-pairs} now looks like this:

@codeblock{
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))
}

@section[#:tag "c2e41"]{Exercise 2.41}

Outside the main procedure I defined a general procedure
@tt{make-ordered-triples} for generating ordered triples with positive integer
values up to @tt{n}:

@codeblock{
(define (make-ordered-triples n)
  (flatmap
   (lambda (i)
     (flatmap
      (lambda (j)
        (map
         (lambda (k)
           (list i j k))
         (enumerate-interval 1 n)))
      (enumerate-interval 1 n)))
   (enumerate-interval 1 n)))
}

This procedure uses a pattern found earlier in @tt{unique-pairs} from
@secref{c2e40}. In the innermost procedure, a list of lists is created with
@tt{map}. Above this, however, nested lists are flattened with @tt{flatmap},
leaving only a singly-nested list of lists at the end. @tt{flatmap} can't be
used at the innermost level, of course, because it would flatten all of the
lists representing the triples into one long list.

The specific filter we are asked to implement, that of finding triples with
distinct values summing to @tt{s}, is defined inside
@tt{ordered-distinct-triples-sum} (which has an unwieldly name if I've ever
seen one...). It checks if the sum of the values of the triple equals @tt{s} by
accumulating @tt{+} from @tt{0}, a basic pattern we've seen before.

@codeblock{
(define (ordered-distinct-triples-sum n s)
  (define (valid-triple? s)
    (lambda (t)
      (and
       (= s (accumulate + 0 t))
       (and (not (= (car t) (cadr t)))
            (not (= (car t) (caddr t)))
            (not (= (cadr t) (caddr t)))))))
  (filter (valid-triple? s) (make-ordered-triples n)))
}

@bold{TODO: Rename that procedure. Really.}

@bold{TODO: General all-distinct? procedure.}

@section[#:tag "c2e42"]{Exercise 2.42}

@bold{I consider this exercise unfinished}

First, a utility procedure for finding if any element in a sequence evaluates
to true:

@codeblock{
(define (any? s)
  (cond ((null? s) #f)
        ((car s) #t)
        (else (any? (cdr s)))))
}

We can use this in the @tt{safe?} procedure, testing if the new piece conflicts
with any of the other ones. This uses a @tt{conflicts?} procedure which tests if
two pieces lay in the same row or on a diagonal.

@codeblock{
(define (safe? k positions)
  (define (conflicts? positions i j)
    (let ((ri (list-ref positions i))
          (rj (list-ref positions j)))
      (or
       (= ri rj)
       (on-diagonal? i ri j rj))))
  (not
   (any?
    (map (lambda (i) (conflicts? positions i (- k 1)))
         (enumerate-interval 0 (- k 2))))))
}

The procedure @tt{on-diagonal?} has an awkward interface and is sort of
obscure. Assuming that the @tt{j} is greater than @tt{i}, it returns true if
either the element in column @tt{j} is in the space @tt{j - i} rows above the
element in row @tt{i} (meaning the piece in column @tt{j} is diagonally up and to
the right from that in @tt{i}), or if the reverse is true and the piece in column
@tt{i} is on an upper diagonal from that in column @tt{j}.

@codeblock{
(define (on-diagonal? i ri j rj)
  ;; assumption: i < j
  (let ((d (- j i)))
    (or
     ;; case 1: diagonal up
     (= rj (+ ri d))
     ;; case 2: diagonal down
     (= ri (+ rj d)))))
}

Simply representing the current queen position as a list with one element per queen,
it is natural that @tt{empty-board} is just an empty list and that @tt{adjoin-position}
just appends the new position to the end of an existing list:

@codeblock{
(define empty-board nil)
}

@codeblock{
(define (adjoin-position new-row k rest-of-queens)
  (append rest-of-queens (list new-row)))
}

@tt{adjoin-position} takes the new column @tt{k} in the given @tt{queens} procedure.
I don't have a use for it.

@section[#:tag "c2e43"]{Exercise 2.43}

@bold{These are just notes}

The essense of what is going wrong here is that @tt{queen-cols} is being called
with the same arguments many times. Basically, in one @tt{(queen-cols k)} call
@tt{(queen-cols (- k 1))} gets evaluated @tt{board-size} times. In general,
@tt{(queen-cols (- k x))} gets evaluated @tt{board-size ^ x} times.

I think I can simplify by not doing a full big-O analysis. The fast @tt{queens}
calls @tt{queen-cols} @tt{board-size} times, once per @tt{k} in
@tt{(enumerate-interval 1 board-size)}. The slow @tt{queens} calls each
@tt{(queen-cols k)} @tt{board-size ^ (board-size - (board-size - k)) - 1} more
times.

Suppose that the fast @tt{queens} solves the eight-queens puzzle in time
@tt{T}. How can I express the amount of time the slow @tt{queens} takes with
respect to @tt{T}?

@bold{TODO: Answer the question}

@section[#:tag "c2e44"]{Exercise 2.44}

@bold{NOTE: Although I could possibly do so, I am not evaluating these
procedures on real painters.}  This means that I will need to study how the
procedures making up the picture language work, and apply them on paper. I
believe this is how these exercises are best done.

Chief among these right now is @tt{below}, since we need to place one painter
on top of another to meet the image specification. We can see by reading the
provided code that @tt{below} can be sketched like this:

@codeblock{
(define (below low high)
;; return a painter with `low' placed below `high'
)
}

For example, notice how @tt{top-left} is the second argument in the first call to
@tt{below} and @tt{bottom-right} is the first argument in the second call:

@codeblock{
(define (corner-split painter n)
;; ...
          (beside (below painter top-left)
                  (below bottom-right corner))
;; ...
)
}

@tt{up-split} is almost identical to @tt{right-split}. Knowing how @tt{below}'s arguments
work, we can use it properly to place the smaller split painters above the original
as specified:

@codeblock{
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))
}

@section[#:tag "c2e45"]{Exercise 2.45}

Writing @tt{up-split}, it should have been apparent that it was almost identical to
@tt{right-split}. Now we generalize it into a general @tt{split} procedure:

@codeblock{
(define (split macro-op micro-op)
  (define (apply-split painter n)
    (if (= n 0)
        painter
        (let ((smaller (apply-split painter (- n 1))))
          (macro-op painter (micro-op smaller smaller)))))
  apply-split)
}

We cannot use a normal @tt{lambda} because the procedure we return has to call itself.
I chose to use an internal @tt{define} to create the procedure and to return it without
calling it after.

@section[#:tag "c2e46"]{Exercise 2.46}

Vectors can simply be another pair joined by @tt{cons}:

@codeblock{
(define (make-vect x y) (cons x y))

(define (xcor-vect v) (car v))

(define (ycor-vect v) (cdr v))
}

The operators @tt{add-vect}, @tt{sub-vect}, and @tt{scale-vect} are similarly trivial:

@codeblock{
(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))
}

@section[#:tag "c2e47"]{Exercise 2.47}

If frames are defined using a list:

@codeblock{
(define (origin-frame frame) (car frame))

(define (edge1-frame frame) (cadr frame))

(define (edge2-frame frame) (caddr frame))
}

If frames are defined by @tt{cons}ing an origin vector onto a pair of edge
vectors:

@codeblock{
(define (origin-frame frame) (car frame))

(define (edge1-frame frame) (cadr frame))

(define (edge2-frame frame) (cddr frame))
}

Since the only difference is the position of the @tt{edge2} vector, the
procedures are all identical except for @tt{edge2-frame}. And these are
almost the same, too, since the data structures are so similar.

@section[#:tag "c2e48"]{Exercise 2.48}

This is rote practice. I think this kind of practice is good for the one
practicing. I used to stumble sometimes with creating the right list structures
in different procedures evaluating in different ways using the different
list-making procedures like @tt{cons} and @tt{append} before rote practice
reinforced my understanding of the list model and the procedures working in it.
However, these exercises are very boring to comment on.

@codeblock{
(define (make-segment start end) (cons start end))

(define (start-segment segment) (car segment))

(define (end-segment segment) (cdr segment))
}

@section[#:tag "c2e49"]{Exercise 2.49}

These procedures are victims to creeping indentation due to the nested
@tt{let}s, but we don't have the tools to do better at the moment.

First, an outline painter:

@codeblock{
(define outline-painter
  (let ((bottom-left (make-vect 0.0 0.0))
        (bottom-right (make-vect 1.0 0.0))
        (top-left (make-vect 0.0 1.0))
        (top-right (make-vect 1 1)))
    (let ((bottom (make-segment bottom-left bottom-right))
          (left (make-segment bottom-left top-left))
          (right (make-segment bottom-right top-right))
          (top (make-segment top-left top-right)))
      (segments->painter
       (list bottom left right top)))))
}

Next, an "X" painter:

@codeblock{
(define x-painter
  (let ((bottom-left (make-vect 0.0 0.0))
        (bottom-right (make-vect 1.0 0.0))
        (top-left (make-vect 0.0 1.0))
        (top-right (make-vect 1.0 1.0)))
    (segments->painter
     (list
      (make-segment bottom-left top-right)
      (make-segment top-left bottom-right)))))
}

I could generalize these two to be applications of a procedure creating
painters working on the corners of a frame, but I won't.

A painter for a diamond whose corners are the midpoints of the frame (using
hardcoded midpoints because we are working on the unit frame):

@codeblock{
(define diamond-painter
  (let ((bottom (make-vect 0.5 0.0))
        (left (make-vect 0.0 0.5))
        (right (make-vect 1.0 0.5))
        (top (make-vect 0.5 1.0)))
    (let ((bottom-left (make-segment bottom left))
          (left-top (make-segment left top))
          (top-right (make-segment top right))
          (right-bottom (make-segment right bottom)))
      (segments->painter
       (list bottom-left left-top top-right right-bottom)))))
}

Defining @tt{wave} is simply a matter of supplying @tt{segments->painter} with
a more complex list of segments. I don't think the amount to be learned here is
worth the work it takes to find these edges based on the images in the book.

@section[#:tag "c2e50"]{Exercise 2.50}

The first, @tt{flip-horiz}, is defined similarly to @tt{flip-vert}, with the new origin
in the bottom-right corner and the edges coming left and up from it:

@codeblock{
(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1 0)
                     (make-vect 0 0)
                     (make-vect 1 1)))
}

The rest are defined in terms of painter transformations we already have:

@codeblock{
(define (rotate180 painter)
  (flip-vert painter))
}

@codeblock{
(define (rotate270 painter)
  (flip-horiz (rotate90 painter)))
}
@section[#:tag "c2e51"]{Exercise 2.51}

We discussed how @tt{below} worked in @secref{c2e44}. Now we implement that.

When defining @tt{below} using calls to @tt{transform-painter}, I have chosen
to diverge from the style used by the given definition of @tt{beside}. I
believe that @tt{beside} makes incomplete use of definition: All vectors are
defined in terms of endpoints except for @tt{split-point}, including a vector
that is the sum of @tt{split-point} and another vector.

As I usually do, I opted for more definitions and abstraction in my
implementation. I've given all of the vectors names, and defined
@tt{split-point} (now named @tt{mid-left} instead of a generic name) using
@tt{scale-vect} for fun. Also, in line with my definition stub seen earlier, I
have named the painter arguments @tt{bottom} and @tt{top} instead of using the
nonspecific names @tt{painter1} and @tt{painter2}.

I believe this procedure is easier to read than the @tt{beside} procedure it is
supposed to be based on.

@codeblock{
(define (below painter-bottom painter-top)
  (let ((bottom-left (make-vect 0.0 0.0))
        (botom-right (make-vect 1.0 0.0))
        (top-left (make-vect 0.0 1.0))
        (mid-left (scale-vect top-left 0.5)))
    (let ((paint-bottom
           (transform-painter painter-bottom
                              bottom-left
                              bottom-right
                              mid-left))
          (paint-top
           (transform-painter painter-top
                              mid-left
                              (add-vect mid-left bottom-right)
                              top-left)))
      (lambda (frame)
        (paint-bottom frame)
        (paint-top frame)))))
}

To define @tt{below} in terms of @tt{beside}, we have to rotate the whole
painter resulting from @tt{beside} and also rotate the painter given to
@tt{beside}.

We know that the first argument of @tt{beside} is painted on the left and the
second on the right. We can imagine then rotating the image by @tt{90} degrees
counterclockwise such that the first painter given to @tt{beside} ends up on
the bottom. We can use @tt{rotate90} for this.

Having rotated the whole painter counterclockwise by @tt{90} degrees, we need
to rotate the painters given to @tt{beside} in such a way that they are facing
upright at the end. This means turning them @tt{90} degrees clockwise (or
@tt{270} counterclockwise), which can be done with @tt{rotate270}.

The whole procedure is as follows:

@codeblock{
(define (below bottom top)
  (rotate90
   (beside (rotate270 bottom) (rotate270 top))))
}

@section[#:tag "c2e52"]{Exercise 2.52}

I did not make the @tt{wave} painter in @secref{c2e49}, so I will not be adding
segments to it.

To modify @tt{corner-split}, I will apply a @tt{flip-horiz} to the base case
and to the bottom-left painter:

@codeblock{
(define (corner-split painter n)
  (if (= n 0)
      (flip-horiz painter)
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up)
              (bottom-right (below up right))
              (corner (corner-split painter (- n 1))))
          (beside (below (flip-horiz painter) top-left)
                  (below bottom-right corner)))))))
}

To modify @tt{square-limit}, I will make the patterns face outward by swapping
the left and right halves in the @tt{square-of-four} call:

@codeblock{
(define (square-limit painter n)
  (let ((combine4 (square-of-four identity flip-horiz
                                  flip-vert rotate180)))
    (combine4 (corner-split painter n))))
}

@bold{TODO: This exercise could really use images}
