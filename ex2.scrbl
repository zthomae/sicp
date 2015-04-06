#lang scribble/manual

@require[scribble/lp]

@title[#:version "" #:style 'toc]{Chapter 2}

@local-table-of-contents[]

@section{Exercise 2.1}

This new @tt{make-rat} procedure is ultimately a case analysis. There are
many ways to do it -- I have chosen one that handles both cases of negative
rational numbers in one case.

@chunk[<make-rat-normalize-sign>
(define (make-rat n d)
  (cond ((and (> n 0) (> d 0))
         (cons n d))
        ((and (< n 0) (< d 0))
         (cons (abs n) (abs d)))
        (else
         (cons (* -1 (abs n)) (abs d)))))
]

@section{Exercise 2.2}

The procedures for making and selecting from line segments and points are very similar --
just operations on pairs of their parts.  @tt{make-segment} and @tt{make-point} are both calls
to @tt{cons}, @tt{start-segment} and @tt{x-point} are both @tt{car}s, and @tt{end-segment} and
@tt{y-point} are both @tt{cdr}s.

@chunk[<make-segment>
(define (make-segment start end)
  (cons start end))
]

@chunk[<start-segment>
(define (start-segment segment)
  (car segment))
]

@chunk[<end-segment>
(define (end-segment segment)
  (cdr segment))
]

@chunk[<make-point>
(define (make-point x y)
  (cons x y))
]

@chunk[<x-point>
(define (x-point point)
  (car point))
]

@chunk[<y-point>
(define (y-point point)
  (cdr point))
]

@tt{midpoint-segment} is a simple operation relying on an @tt{average} procedure that
is trivial enough to not include here.

@chunk[<midpoint-segment>
(define (midpoint-segment s)
  (let ((x (average (x-point (start-segment s))
                    (x-point (end-segment s))))
        (y (average (y-point (start-segment s))
                    (y-point (end-segment s)))))
    (make-point x y)))
]

To illustrate the power of these similar means of abstraction even more, we can
generalize the @tt{print-point} procedure. The original as listed in the exercise
is as follows:

@chunk[<print-point-book>
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))
]

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

@chunk[<print-pair?>
(define (print-pair pair pre between end inner-print)
  (display pre)
  (display (car pair))
  (display between)
  (display (cdr pair))
  (display end))
]

However, while this will work perfectly well for reimplementing @tt{print-point},
it works less than well for making @tt{print-segment}. It would be desirable to
have @tt{print-segment} call @tt{print-point} on each of its points, but @tt{print-pair}
will necessarily call @tt{display} on them, printing them with the environment's
default representation for pairs. This will give a readable printout, but I believe
we should do better. This can be done by supplying one more argument to print-pair,
an @tt{inner-print} procedure that is called to print the @tt{car} and @tt{cdr} of the pair
And so we get the actual @tt{print-pair} procedure:

@chunk[<print-pair>
(define (print-pair pair pre between end inner-print)
  (display pre)
  (inner-print (car pair))
  (display between)
  (inner-print (cdr pair))
  (display end))
]

And with this, we can make @tt{print-point} and @tt{print-segment}:

@chunk[<print-point>
(define (print-point p)
  (print-pair p "(" "," ")" display))
]

@chunk[<print-segment>
(define (print-segment s)
  (print-pair s "" " -> " "" print-point))
]

There is another benefit that we can get out of this. Suppose we implemented
@tt{print-segment} using the first version of @tt{print-point} given in the book.
Each point would have a newline before it, necessarily and always. If we wanted
to change this (say, to print a segment on one line), we would have to write a
new @tt{print-point} procedure. This is significantly easier to handle with the new
@tt{print-pair} procedure -- we can simply supply newlines to be printed wherever
we want. The problem could be solved like so:

@chunk[<println-segment>
(define (println-segment s)
  (print-pair s "" " -> " "\n" print-point))
]

It would be even better if the @tt{car}s and @tt{cdr}s of the pairs sent to
@tt{print-pair} knew how to print themselves, but that is getting beyond the scope
of the exercise.

@section{Exercise 2.3}

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

@chunk[<make-rectangle-segments>
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
]

This procedure is, after error-checking to make sure both the width and height
segments begin from the origin and that they are both parallel to their
respective axes, just a @tt{cons} call like the others.

The segment selectors are just as simple as the others. But in order to
calculate the area and perimeter of the rectangle, we need to get the
width and height of the rectangle as numbers. By extracting the coordinates
from the ends of the width and height segments, we can do this:

@chunk[<get-width-segment>
(define (get-width-segment rect) (car rect))

(define (get-width rect)
  (x-point (end-segment (get-width-segment rect))))
]

@chunk[<get-height-segment>
(define (get-height-segment rect) (cdr rect))

(define (get-height rect)
  (y-point (end-segment (get-height-segment rect))))
]

Then we can calculate the area and perimeter using these @tt{get-width} and
@tt{get-height} procedures:

@chunk[<area>
(define (area rect)
  (* (get-width rect) (get-height rect)))
]

@chunk[<perimeter>
(define (perimeter rect)
  (* 2 (+ (get-width rect) (get-height rect))))
]

Now let's move on to our second representation, using a bottom ("width") corner
and a top ("height") corner -- or perhaps it's better to describe this as being
made of the @tt{end-segment} points from the width and height segments used in
the last representation. Since it takes less complex data in its construction,
the new procedure @tt{make-rectangle-points} is simpler in providing the same
guarantees as @tt{make-rectangle-segments}, because it has no origins points in
line segments to check.

@chunk[<make-rectangle-points>
(define (make-rectangle-points width-corner height-corner)
  (cond ((not (= 0 (y-point width-corner)))
         (error ("width corner must have no y component")))
        ((not (= 0 (x-point height-corner)))
         (error ("height corner must have no x component")))
        (else (cons width-corner height-corner))))
]

Getting the width and height of the rectangle is also easier, since there
are fewer data structures to traverse:

@chunk[<get-width-corner>
(define (get-width-corner rect)
  (car rect))

(define (get-width rect)
  (x-point (get-width-corner rect)))
]

@chunk[<get-height-corner>
(define (get-height-corner rect)
  (cdr rect))

(define (get-height rect)
  (y-point (get-height-corner rect)))
]

And with that, we can use @tt{area} and @tt{perimeter} just as before.

In a real implementation, it would be better to hide references to the
implementation-specific details, like @tt{-segment} and @tt{-corner}, behind
standard interfaces like @tt{make-rectangle}, @tt{get-width}, and
@tt{get-height}. More on this topic will be introduced later in the book.

@section{Exercise 2.4}

The definitions of @tt{cons} and @tt{car} we are given are as follows:

@chunk[<cons-2.4>
(define (cons x y)
  (lambda (m) (m x y)))
]

@chunk[<car-2.4>
(define (car z)
  (z (lambda (p q) p)))
]

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

@chunk[<cdr-2.4>
(define (cdr z)
  (z (lambda (p q) q)))
]

Using the substitution model to evaluate:

@verbatim{
(cdr z)
(z (lambda (p q) q))
((lambda (m) (m x y)) (lambda (p q) q))
((lambda (p q) q) x y)
y
}

@section{Exercise 2.5}

@bold{TODO}

@section{Exercise 2.6}

@bold{TODO: Words}

@chunk[<zero>
(define zero (lambda (f) (lambda (x) x)))
]

@chunk[<add-1>
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
]

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

@chunk[<one>
(define one (lambda (f) (lambda (x) (f x))))
]

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

@chunk[<two>
(define two (lambda (f) (lambda (x) (f (f x)))))
]

@chunk[<add-church>
(define (add-church m n)
  (lambda (f)
    (lambda (x)
      ((m f) ((n f) x)))))
]

@section{Exercise 2.7}

We can see by examining the implementations of the operations that @tt{make-interval}
takes the lower bound as the first parameter and the upper bound as the second. For
example, in @tt{add-interval}:

@chunk[<add-interval>
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
]

Therefore, knowing that @tt{make-interval} is a @tt{cons} call, we know to define
@tt{lower-bound} and @tt{upper-bound} as so:

@chunk[<lower-bound>
(define (lower-bound i) (car i))
]

@chunk[<upper-bound>
(define (upper-bound i) (cdr i))
]

@section{Exercise 2.8}

Alyssa reasoned that adding two intervals made a new interval where the lower
bound was the sum of the arguments' lower bounds and the upper bound was the
sum of the arguments' upper bounds. We can define a similar procedure
@tt{sub-interval} by supposing that subtracting two intervals creates an interval
where the lower bound is the difference between the two lower bounds (and similarly
for the upper bound).

@chunk[<sub-interval>
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (lower-bound x) (lower-bound y))))
]

@section{Exercise 2.9}

First, a definition of the @tt{width} procedure:

@chunk[<width>
(define (width x) (/ (- (upper-bound x) (lower-bound x)) 2))
]

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

@section{Exercise 2.10}

Before modifying any code, we need to understand: Why does it not make sense
to divide by an interval that spans zero?

When in doubt, we can manipulate interval with our procedures to investigate.
But first, a procedure for computing the reciprocal of an interval:

@chunk[<reciprocal>
(define (reciprocal i)
  (make-interval (/ 1.0 (upper-bound i))
                 (/ 1.0 (lower-bound i))))
]

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

@chunk[<div-interval-safe>
(define (div-interval x y)
  (if (and (< (lower-bound y) 0) (> (upper-bound y) 0))
      (error "Cannot divide by an interval spanning zero")
      (mul-interval x (reciprocal y))))
]

@section{Exercise 2.11}

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

@chunk[<equal-intervals?>
(define (equal-intervals? x y)
  (and
   (= (lower-bound x) (lower-bound y))
   (= (upper-bound x) (upper-bound y))))
]

We can use this to test if multiplied intervals by two different procedures are
equal. Applying this to every combination of signs for the bounds of two intervals
(whose bounds are chosen arbitrarily, and should not be important), we can make a
procedure to exhaustively test the nine sign combination cases to make sure that
our new procedure gets the same result as the old one every time.

@chunk[<test-new-mul-interval>
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
]

The code is not pretty, but it works, and will pinpoint exactly what cases are
in error. I made mistakes when I first wrote the new multiplication procedure,
and this was helpful.

Below is the new multiplication procedure using a reduced number of multiplications.
It uses a @tt{cond} statement checking combinations of signs for all bounds (and as
a stylistic choice, leaves the case with more than @tt{2} multiplications for the
@tt{else} case).

@chunk[<mul-interval-ben>
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
]

And using our testing procedure, we can verify that this procedure is correct:

@verbatim{
(test-new-mul-interval mul-interval-ben)
=> #t
}

@section{Exercise 2.12}

@tt{make-center-percent} is not hard to implement. Just calculate the width
from the center and the percent and then use @tt{make-center-width}:

@chunk[<make-center-percent>
(define (make-center-percent c p)
  (let ((w (* c (/ p 100))))
    (make-center-width c w)))
]

@tt{percent} can be calculated by subtracting the center from the upper bound
and then dividing by the width:

@chunk[<percent>
(define (percent i)
  (let ((c (center i))
        (w (width i)))
    (/ (- (upper-bound i) c) w)))
]

@section{Exercise 2.13}

@bold{TODO}

@section{Exercise 2.14}

@bold{TODO}

@section{Exercise 2.15}

@bold{TODO}

@section{Exercise 2.16}

@bold{TODO}

@section{Exercise 2.17}

@bold{TODO: Words}

@chunk[<last-pair>
(define (last-pair l)
  (if (null? (cdr l)) l
      (last-pair (cdr l))))
]

@section{Exercise 2.18}

@bold{TODO: Words}

@chunk[<reverse>
(define (reverse l)
  (if (null? (cdr l)) l
      (append (reverse (cdr l)) (list (car l)))))
]

@section{Exercise 2.19}

@tt{first-denomination}, @tt{except-first-denomination}, and @tt{no-more?} are all primitive
operations on lists:

@chunk[<first-denomination>
(define (first-denomination coin-values)
  (car coin-values))
]

@chunk[<except-first-denomination>
(define (except-first-denomination coin-values)
  (cdr coin-values))
]

@chunk[<no-more?>
(define (no-more? coin-values)
  (null? coin-values))
]

The order of the list @tt{coin-values} does not affect the answer of @tt{cc}:

@verbatim{
(= (cc 100 us-coins) (cc 100 (reverse us-coins)))
=> #t
}

@section{Exercise 2.20}

I found it easiest to define @tt{same-parity} iteratively, using an iterative inner
procedure taking the first argument, the results list, and the rest of the numbers
to check as arguments.

@chunk[<same-parity>
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
]

One of the reasons I chose to do it this way is that @tt{same-parity} expects
its arguments to be individual, rather than in the form of a list. I could
alternatively have used @tt{apply} to get around this:

@chunk[<same-parity-recur>
(define (same-parity x . xs)
  (define (is-same-parity? x y)
    (= (remainder x 2) (remainder y 2)))
  (cond ((null? xs) nil)
        ((is-same-parity? x (car xs))
         (cons (car xs) (apply same-parity (cons x (cdr xs)))))
        (else
         (apply same-parity (cons x (cdr xs))))))
]

However, although I have used it before, we technically don't know @tt{apply}
at this point in the book.

@section{Exercise 2.21}

First, the implementation of @tt{square-list} that does not use @tt{map}:

@chunk[<square-list-no-map>
(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items))
            (square-list (cdr items)))))
]

And the implementation that does:

@chunk[<square-list-map>
(define (square-list items)
  (map (lambda (x) (square x)) items))
]

@section{Exercise 2.22}

The first procedure produces the answer list in the wrong order because it
@tt{cons}es the square of the current entry to the front of the @tt{answers}
list. You can do this and get the result list in the correct order when in a
recursive process because the @tt{cons} calls will be evaluated in reverse
order, appending earlier items in the list to the front. However, when in an
iterative process, you append to the front of the list in order of traversal,
meaning that later items are squared and appear earlier in the list.

The second procedure does not work because @tt{cons}es a list to a value,
which does not produce a valid list structure.

@section{Exercise 2.23}

I have chosen to use @tt{nil} as the return value rather than true. This isn't
exactly what I'd like to do, but I don't think @tt{for-each} should have a
return value, and at this time I think this is closer to that.

@chunk[<for-each>
(define (for-each f items)
  (if (null? items) nil
      ((lambda ()
         (f (car items))
         (for-each f (cdr items))))))
]

@section{Exercise 2.24}

Omitted.

@section{Exercise 2.25}

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

@section{Exercise 2.26}

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

@section{Exercise 2.27}

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

@chunk[<deep-reverse>
(define (deep-reverse l)
  (cond
   ((not (pair? l)) l)
   ((null? (cdr l)) (deep-reverse (car l)))
   (else (cons (deep-reverse (cdr l)) (list (deep-reverse (car l)))))))
]

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

@section{Exercise 2.28}

@tt{fringe} is similar to @tt{deep-reverse} in that it has three base cases for
non-lists, single-item lists, and general lists. However, the non-pair case
produces a list, and the general case uses @tt{append} to keep the resulting
list flat. Avoiding @tt{append}ing @tt{nil} to a list is still done, of course.

@chunk[<fringe>
(define (fringe l)
  (cond ((not (pair? l)) (list l))
        ((null? (cdr l)) (fringe (car l)))
        (else (append (fringe (car l)) (fringe (cdr l))))))
]

@section{Exercise 2.29}

@bold{TODO: Words}

@chunk[<left-branch>
(define (left-branch mobile)
  (car mobile))
]

@chunk[<right-branch>
(define (right-branch mobile)
  (cadr mobile))
]

@chunk[<branch-length>
(define (branch-length branch)
  (car branch))
]

@chunk[<branch-structure>
(define (branch-structure branch)
  (cadr branch))
]

@chunk[<total-weight>
(define (weigh-branch branch)
    (let ((s (branch-structure branch)))
      (if (not (list? s)) s
          (total-weight s))))

(define (total-weight mobile)
  (+ (weigh-branch (left-branch mobile)) (weigh-branch (right-branch mobile))))
]

@chunk[<is-mobile-balanced?>
(define (is-mobile-balanced? mobile)
  (define (torque branch)
    (* (branch-length branch) (weigh-branch branch)))
  (define (are-submobiles-balanced? branch)
    (let ((s (branch-structure branch)))
      ;; if s is not a mobile, then all submobiles are balanced
      (if (not (list? s)) #t
          (is-mobile-balanced s))))
  (let ((left (left-branch mobile))
        (right (right-branch mobile)))
    (if (not (= (torque left) (torque right))) #f
        (and
         (are-submobiles-balanced? left)
         (are-submobiles-balanced? right)))))
]

@bold{TODO: Testing}

@bold{TODO: Changing list to cons (just change list? to pair?, I think)}

@section{Exercise 2.30}

These procedures are direct modifications of the corresponding ones for
@tt{scale-tree}. This is relevant to the next exercise.

@chunk[<square-tree-direct>
(define (square-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))
]

@chunk[<square-tree-map>
(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (square sub-tree)))
       tree))
]

@section{Exercise 2.31}

@chunk[<tree-map>
(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map proc sub-tree)
             (proc sub-tree)))
       tree))
]

As an example, we could define @tt{scale-tree} this way:

@verbatim{
(define (scale-tree tree factor)
  (tree-map (lambda (x) (* x factor)) tree))
}

@section{Exercise 2.32}

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

@chunk[<subsets>
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append
         rest
         (map (lambda (subset) (append subset (list (car s))))
              rest)))))
]

@section{Exercise 2.33}

