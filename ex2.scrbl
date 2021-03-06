#lang scribble/manual

@require[scribble/lp]
@require[scribble/examples]
@require["eval.rkt"]

@title[#:version "" #:style 'toc]{Chapter 2}

@local-table-of-contents[]

@define[ev @make-eval[]]

@section[#:tag "c2e1"]{Exercise 2.1}

This new @tt{make-rat} procedure is ultimately a case analysis. There are
many ways to do it -- I have chosen one that handles both cases of negative
rational numbers in one case.

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (make-rat n d)
   (cond ((and (> n 0) (> d 0))
          (cons n d))
         ((and (< n 0) (< d 0))
          (cons (abs n) (abs d)))
         (else
          (cons (* -1 (abs n)) (abs d)))))
 ]

@section[#:tag "c2e2"]{Exercise 2.2}

The procedures for making and selecting from line segments and points are very similar --
just operations on pairs of their parts.  @tt{make-segment} and @tt{make-point} are both calls
to @tt{cons}, @tt{start-segment} and @tt{x-point} are both @tt{car}s, and @tt{end-segment} and
@tt{y-point} are both @tt{cdr}s.

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (make-segment start end)
   (cons start end))
 ]

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (start-segment segment)
   (car segment))
 ]

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (end-segment segment)
   (cdr segment))
 ]

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (make-point x y)
   (cons x y))
 ]

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (x-point point)
   (car point))
 ]

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (y-point point)
   (cdr point))
 ]

@tt{midpoint-segment} is a simple operation relying on an @tt{average} procedure that
is trivial enough to not include here.

@examples[
 #:label #f #:eval ev #:no-prompt
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

@examples[
 #:label #f #:eval ev #:no-prompt
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

@examples[
 #:label #f #:eval ev #:no-prompt
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

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (print-pair pair pre between end inner-print)
   (display pre)
   (inner-print (car pair))
   (display between)
   (inner-print (cdr pair))
   (display end))
 ]

And with this, we can make @tt{print-point} and @tt{print-segment}:

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (print-point p)
   (print-pair p "(" "," ")" display))
 ]

@examples[
 #:label #f #:eval ev #:no-prompt
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

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (println-segment s)
   (print-pair s "" " -> " "\n" print-point))
 ]

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

@examples[
 #:label #f #:eval ev #:no-prompt
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

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (get-width-segment rect) (car rect))

 (define (get-width rect)
   (x-point (end-segment (get-width-segment rect))))
 ]

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (get-height-segment rect) (cdr rect))

 (define (get-height rect)
   (y-point (end-segment (get-height-segment rect))))
 ]

Then we can calculate the area and perimeter using these @tt{get-width} and
@tt{get-height} procedures:

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (area rect)
   (* (get-width rect) (get-height rect)))
 ]

@examples[
 #:label #f #:eval ev #:no-prompt
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

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (make-rectangle-points width-corner height-corner)
   (cond ((not (= 0 (y-point width-corner)))
          (error ("width corner must have no y component")))
         ((not (= 0 (x-point height-corner)))
          (error ("height corner must have no x component")))
         (else (cons width-corner height-corner))))
 ]

Getting the width and height of the rectangle is also easier, since there
are fewer data structures to traverse:

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (get-width-corner rect)
   (car rect))

 (define (get-width rect)
   (x-point (get-width-corner rect)))
 ]

@examples[
 #:label #f #:eval ev #:no-prompt
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

@section[#:tag "c2e4"]{Exercise 2.4}

The definitions of @tt{cons} and @tt{car} we are given are as follows:

@define[cons-ev @make-eval[]]

@examples[
 #:label #f #:eval cons-ev #:no-prompt
 (define (cons x y)
   (lambda (m) (m x y)))
 ]

@examples[
 #:label #f #:eval cons-ev #:no-prompt
 (define (car z)
   (z (lambda (p q) p)))
 ]

@tt{cons} is a procedure of two arguments, @tt{x} and @tt{y}, that returns a
procedure of one argument, @tt{m}, that calls @tt{m} on @tt{x} and @tt{y}.
@tt{car} is a procedure taking a procedure @tt{z} as its only argument, and it
calls @tt{z} on a procedure that returns the first of two arguments given to it.

We can see how this evaluates by substituting:

@racketblock[
 (car z)
 (z (lambda (p q) p))
 ((lambda (m) (m x y)) (lambda (p q) p))
 ((lambda (p q) p) x y)
 x
]

We know that @tt{(car z)} should return @tt{x} by definition, so we can conclude
that @tt{car} is correct.

We can do similarly for @tt{cdr}. Its definition is almost identical to @tt{car}'s,
except it passes a procedure returning its second of two arguments.

@examples[
 #:label #f #:eval cons-ev #:no-prompt
 (define (cdr z)
   (z (lambda (p q) q)))
 ]

Using the substitution model to evaluate:

@racketblock[
 (cdr z)
 (z (lambda (p q) q))
 ((lambda (m) (m x y)) (lambda (p q) q))
 ((lambda (p q) q) x y)
 y
]

@section[#:tag "c2e5"]{Exercise 2.5}

@bold{TODO}

@section[#:tag "c2e6"]{Exercise 2.6}

@bold{TODO: Words}

@examples[
 #:label #f #:eval ev #:no-prompt
 (define zero (lambda (f) (lambda (x) x)))
 ]

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (add-1 n)
   (lambda (f) (lambda (x) (f ((n f) x)))))
 ]

@racketblock[
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
]

@racketblock[
 (define one (lambda (f) (lambda (x) (f x))))
]

@racketblock[
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
]

@examples[
 #:label #f #:eval ev #:no-prompt
 (define two (lambda (f) (lambda (x) (f (f x)))))
 ]

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (add-church m n)
   (lambda (f)
     (lambda (x)
       ((m f) ((n f) x)))))
 ]

@section[#:tag "c2e7"]{Exercise 2.7}

We can see by examining the implementations of the operations that @tt{make-interval}
takes the lower bound as the first parameter and the upper bound as the second. For
example, in @tt{add-interval}:

@examples[
 #:label #f #:eval ev #:hidden
 (define (make-interval a b) (cons a b))
 ]

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (add-interval x y)
   (make-interval (+ (lower-bound x) (lower-bound y))
                  (+ (upper-bound x) (upper-bound y))))
 ]

Therefore, knowing that @tt{make-interval} is a @tt{cons} call, we know to define
@tt{lower-bound} and @tt{upper-bound} as so:

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (lower-bound i) (car i))
 ]

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (upper-bound i) (cdr i))
 ]

@section[#:tag "c2e8"]{Exercise 2.8}

Alyssa reasoned that adding two intervals made a new interval where the lower
bound was the sum of the arguments' lower bounds and the upper bound was the
sum of the arguments' upper bounds. We can define a similar procedure
@tt{sub-interval} by supposing that subtracting two intervals creates an interval
where the lower bound is the difference between the two lower bounds (and similarly
for the upper bound).

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (sub-interval x y)
   (make-interval (- (lower-bound x) (lower-bound y))
                  (- (lower-bound x) (lower-bound y))))
 ]

@section[#:tag "c2e9"]{Exercise 2.9}

First, a definition of the @tt{width} procedure:

@examples[
 #:label #f #:eval ev #:no-prompt
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

@examples[
 #:label #f #:eval ev #:hidden
 (define (mul-interval x y)
   (let ((p1 (* (lower-bound x) (lower-bound y)))
         (p2 (* (lower-bound x) (upper-bound y)))
         (p3 (* (upper-bound x) (lower-bound y)))
         (p4 (* (upper-bound x) (upper-bound y))))
     (make-interval (min p1 p2 p3 p4)
                    (max p1 p2 p3 p4))))

 (define (div-interval x y)
   (mul-interval x
                 (make-interval (/ 1.0 (upper-bound y))
                                (/ 1.0 (lower-bound y)))))
 ]

@examples[
 #:label #f #:eval ev
 (define i1 (make-interval 0 5))
 (define i2 (make-interval 5 10))
 (= (width i1) (width i2))

 (define i3 (make-interval 1 10))

 (= (width (mul-interval i1 i3)) (width (mul-interval i2 i3)))

 (= (width (div-interval i1 i3)) (width (div-interval i2 i3)))
 ]

@section[#:tag "c2e10"]{Exercise 2.10}

Before modifying any code, we need to understand: Why does it not make sense
to divide by an interval that spans zero?

When in doubt, we can manipulate interval with our procedures to investigate.
But first, a procedure for computing the reciprocal of an interval:

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (reciprocal i)
   (make-interval (/ 1.0 (upper-bound i))
                  (/ 1.0 (lower-bound i))))
 ]

Now we can do the following:

@examples[
 #:label #f #:eval ev
 (define i1 (make-interval 1 10))
 (define i2 (make-interval -5 5))

 (define ri1 (reciprocal i1))
 (define ri2 (reciprocal i2))

 ri1

 ri2
 ]

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

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (div-interval x y)
   (if (and (< (lower-bound y) 0) (> (upper-bound y) 0))
       (error "Cannot divide by an interval spanning zero")
       (mul-interval x (reciprocal y))))
 ]

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

@examples[
 #:label #f #:eval ev #:no-prompt
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

@examples[
 #:label #f #:eval ev #:no-prompt
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

@examples[
 #:label #f #:eval ev #:no-prompt
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

@examples[
 #:label #f #:eval ev
 (test-new-mul-interval mul-interval-ben)
 ]

@section[#:tag "c2e12"]{Exercise 2.12}

@tt{make-center-percent} is not hard to implement. Just calculate the width
from the center and the percent and then use @tt{make-center-width}:

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (make-center-percent c p)
   (let ((w (* c (/ p 100))))
     (make-center-width c w)))
 ]

@tt{percent} can be calculated by subtracting the center from the upper bound
and then dividing by the width:

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (percent i)
   (let ((c (center i))
         (w (width i)))
     (/ (- (upper-bound i) c) w)))
 ]

@section[#:tag "c2e13"]{Exercise 2.13}

@bold{TODO}

@section[#:tag "c2e14"]{Exercise 2.14}

@bold{TODO}

@section[#:tag "c2e15"]{Exercise 2.15}

@bold{TODO}

@section[#:tag "c2e16"]{Exercise 2.16}

@bold{TODO}

@section[#:tag "c2e17"]{Exercise 2.17}

To get the last pair of a given list (assuming it is nonempty, as indicated), we can
recursively call @tt{last-pair} on successive @tt{cdr}s of the list until the @tt{cdr}
is null:

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (last-pair l)
   (if (null? (cdr l)) l
       (last-pair (cdr l))))
 ]

@section[#:tag "c2e18"]{Exercise 2.18}

To implement @tt{reverse}, we will use an inner tail-recursive procedure that takes
the accumulated reversed list and the unreversed portion of the original list as arguments.

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (reverse l)
   (define (inner acc rest)
     (if (null? rest)
         acc
         (inner (cons (car rest) acc) (cdr rest))))
   (inner '() l))
 ]

@section[#:tag "c2e19"]{Exercise 2.19}

@tt{first-denomination}, @tt{except-first-denomination}, and @tt{no-more?} are all primitive
operations on lists:

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (first-denomination coin-values)
   (car coin-values))
 ]

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (except-first-denomination coin-values)
   (cdr coin-values))
 ]

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (no-more? coin-values)
   (null? coin-values))
 ]

The order of the list @tt{coin-values} does not affect the answer of @tt{cc}:

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (cc amount coin-values)
   (cond ((= amount 0) 1)
         ((or (< amount 0) (no-more? coin-values)) 0)
         (else
          (+ (cc amount
                 (except-first-denomination coin-values))
             (cc (- amount
                    (first-denomination coin-values))
                 coin-values)))))
 ]

@examples[
 #:label #f #:eval ev
 (define us-coins (list 50 25 10 5 1))
 (= (cc 100 us-coins) (cc 100 (reverse us-coins)))
 ]

@section[#:tag "c2e20"]{Exercise 2.20}

I found it easiest to define @tt{same-parity} iteratively, using an iterative inner
procedure taking the first argument, the results list, and the rest of the numbers
to check as arguments.

@examples[
 #:label #f #:eval ev #:no-prompt
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

@examples[
 #:label #f #:eval ev #:no-prompt
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

@section[#:tag "c2e21"]{Exercise 2.21}

First, the implementation of @tt{square-list} that does not use @tt{map}:

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (square-list items)
   (if (null? items)
       nil
       (cons (square (car items))
             (square-list (cdr items)))))
 ]

And the implementation that does:

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (square-list items)
   (map (lambda (x) (square x)) items))
 ]

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

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (for-each f items)
   (if (null? items) nil
       ((lambda ()
          (f (car items))
          (for-each f (cdr items))))))
 ]

@section[#:tag "c2e24"]{Exercise 2.24}

Omitted.

@section[#:tag "c2e25"]{Exercise 2.25}

I give solutions both in reduced and unreduced form.

@examples[
 #:label #f #:eval ev
 (define 25-1 (list 1 3 (list 5 7) 9))
 (= 7 (car (cdr (car (cdr (cdr 25-1))))))

 (= 7 (car (cdaddr 25-1)))

 (define 25-2 (list (list 7)))
 (= 7 (car (car 25-2)))

 (= 7 (caar 25-2))

 (define 25-3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
 (= 7 (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr 25-3)))))))))))))

 (= 7 (cadadr (cadadr (cadadr 25-3))))
 ]

@section[#:tag "c2e26"]{Exercise 2.26}

@examples[
 #:label #f #:eval ev
 (define x (list 1 2 3))
 (define y (list 4 5 6))
 (append x y)
 ]

We get this result because @tt{append} takes two lists and returns a new list
containing all the items from the first list followed by all the items from
the second list.

@examples[
 #:label #f #:eval ev
 (cons x y)
 ]

We get this result because @tt{cons} creates a pair where the first element points
to the list @tt{(1 2 3)} and is  is the valid list @tt{(4 5 6)}.

@examples[
 #:label #f #:eval ev
 (list x y)
 ]

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

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (deep-reverse l)
   (cond
     ((not (pair? l)) l)
     ((null? (cdr l)) (deep-reverse (car l)))
     (else (cons (deep-reverse (cdr l)) (list (deep-reverse (car l)))))))
 ]

There need to be three cases here because even though @tt{(deep-reverse (cdr l))} will
return @tt{nil} if that's what @tt{(cdr l)} is, @tt{cons}ing @tt{nil} in front will
create the wrong list structure. In other words,

@examples[
 #:label #f #:eval ev
 (define (deep-reverse l)
   (cond
     ((not (pair? l)) l)
     ;((null? (cdr l)) (deep-reverse (car l)))
     (else (cons (deep-reverse (cdr l)) (list (deep-reverse (car l)))))))

 (deep-reverse x)
 ]

@section[#:tag "c2e28"]{Exercise 2.28}

@tt{fringe} is similar to @tt{deep-reverse} in that it has three base cases for
non-lists, single-item lists, and general lists. However, the non-pair case
produces a list, and the general case uses @tt{append} to keep the resulting
list flat. Avoiding @tt{append}ing @tt{nil} to a list is still done, of course.

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (fringe l)
   (cond ((not (pair? l)) (list l))
         ((null? (cdr l)) (fringe (car l)))
         (else (append (fringe (car l)) (fringe (cdr l))))))
 ]

@section[#:tag "c2e29"]{Exercise 2.29}

First of all, writing the selectors @tt{left-branch} and @tt{right-branch} is trivial,
as well as @tt{branch-length} and @tt{branch-structure}:

@examples[
 #:label #f #:eval ev #:no-prompt
 (define left-branch car)
 ]

@examples[
 #:label #f #:eval ev #:no-prompt
 (define right-branch cadr)
 ]

@examples[
 #:label #f #:eval ev #:no-prompt
 (define branch-length car)
 ]

@examples[
 #:label #f #:eval ev #:no-prompt
 (define branch-structure cadr)
 ]

Finding the total weight of a branch is just a matter of summing the weights of the
left and right branches recursively. We can do that using two procedures, @tt{total-weight}
and @tt{weigh-branch}, both of which call each other:

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (weigh-branch branch)
   (let ((s (branch-structure branch)))
     (if (not (list? s)) s
         (total-weight s))))

 (define (total-weight mobile)
   (+ (weigh-branch (left-branch mobile)) (weigh-branch (right-branch mobile))))
 ]

To determine if a mobile is balanced, we can follow the definition of a balanced mobile
and define a recursive procedure that tests whether two branches have equal torque (where
@tt{torque} is computed by an inner procedure) and, if so, recursively checks if the
mobiles rooted at each branch are balanced:

@examples[
 #:label #f #:eval ev #:no-prompt
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
 ]

Supposing that we changed the representation of mobiles to use @tt{cons} cells instead of
lists, we would only need to change the selectors @tt{right-branch} and @tt{branch-structure}
to use @tt{cdr} instead of @tt{cadr}, and also change the @tt{list?} checks to @tt{pair?} checks.
(We could also use @tt{pair?} checks in this implementation, since lists are pairs, but I believe
it is better to use the more restrictive selector).

@section[#:tag "c2e30"]{Exercise 2.30}

These procedures are direct modifications of the corresponding ones for
@tt{scale-tree}. This is relevant to the next exercise.

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (square-tree tree)
   (cond ((null? tree) nil)
         ((not (pair? tree)) (square tree))
         (else (cons (square-tree (car tree))
                     (square-tree (cdr tree))))))
 ]

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (square-tree tree)
   (map (lambda (sub-tree)
          (if (pair? sub-tree)
              (square-tree sub-tree)
              (square sub-tree)))
        tree))
 ]

@section[#:tag "c2e31"]{Exercise 2.31}

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (tree-map proc tree)
   (map (lambda (sub-tree)
          (if (pair? sub-tree)
              (tree-map proc sub-tree)
              (proc sub-tree)))
        tree))
 ]

As an example, we could define @tt{scale-tree} this way:

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (scale-tree tree factor)
   (tree-map (lambda (x) (* x factor)) tree))
 ]

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

@racketblock[
 (lambda (subset)
 (append subset (list (car s))))
]

The complete procedure is below.

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (subsets s)
   (if (null? s)
       (list nil)
       (let ((rest (subsets (cdr s))))
         (append
          rest
          (map (lambda (subset) (append subset (list (car s))))
               rest)))))
 ]

@section[#:tag "c2e33"]{Exercise 2.33}

To implement @tt{map} using @tt{accumulate}, we give @tt{accumulate} a
procedure @tt{cons}ing the result of applying @tt{p} to its first argument
(the new value) onto its second argument (the accumulated list so far).
And since @tt{accumulate} evolves a recursive process, the values of the
new sequence will be in the right order.

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (accumulate op initial sequence)
   (if (null? sequence)
       initial
       (op (car sequence)
           (accumulate op initial (cdr sequence)))))

 (define (map p sequence)
   (accumulate (lambda (x y) (cons (p x) y)) nil sequence))
 ]

To implement @tt{append}, we set up one sequence as the initial value and
apply @tt{cons} to successive elements from the other. The second argument
is used as the initial value because @tt{cons} will append new values to
the front of the list.
@examples[
 #:label #f #:eval ev #:no-prompt
 (define (append seq1 seq2)
   (accumulate cons seq2 seq1))
 ]

To implement @tt{length}, we give a procedure that adds 1 to its second argument.

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (length sequence)
   (accumulate (lambda (x y) (+ y 1)) 0 sequence))
 ]

@section[#:tag "c2e34"]{Exercise 2.34}

In every step, we add @tt{this-coeff} to the product of the already-computed
@tt{higher-terms} and @tt{x}. With an initial term of @tt{0}, this procedure
readily applies itself to a use of @tt{accumulate}.

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (horner-eval x coefficient-sequence)
   (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* higher-terms x)))
               0
               coefficient-sequence))
 ]

@section[#:tag "c2e35"]{Exercise 2.35}

I can think of a silly way to write @tt{count-leaves} using the template provided
by the exercise:

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (count-leaves t)
   (accumulate (lambda (x y) (+ y 1))
               0
               (map identity (enumerate-tree t))))
 ]

Of course, this could be simplified like this:

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (count-leaves t)
   (accumulate (lambda (x y) (+ y 1)) 0 (enumerate-tree t)))
 ]

However, we don't have to write @tt{count-leaves} as an accumulation, because
we already have an @tt{accumulate} procedure for counting things: @tt{length}
from @secref{c2e33}:

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (count-leaves t)
   (length (enumerate-tree t)))
 ]

@section[#:tag "c2e36"]{Exercise 2.36}

The added parts of the procedure are both based on a simple idea: To get the
first element of each of @tt{seqs}, you can @tt{map} @tt{car} over them. And
to get the rest of each of them, you can @tt{map} @tt{cdr} over them.

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (accumulate-n op init seqs)
   (if (null? (car seqs))
       nil
       (cons (accumulate op init (map car seqs))
             (accumulate-n op init (map cdr seqs)))))
 ]

@section[#:tag "c2e37"]{Exercise 2.37}

@tt{matrix-*-matrix} maps over each row of the matrix a procedure that maps
multiplication by the scalar @tt{v} over the entries in the row.

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (matrix-*-vector m v)
   (map (lambda (r) (map (lambda (e) (* e v)) r)) m))
 ]

@tt{transpose} uses @tt{cons} with @tt{accumulate-n} to create a list of lists
made up of the columns of the original matrix.

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (transpose mat)
   (accumulate-n cons nil mat))
 ]

@tt{matrix-*-matrix} computes the @tt{dot-product} of each row of @tt{m} with
each column of @tt{n} (where the columns are found with @tt{transpose}).

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (matrix-*-matrix m n)
   (let ((cols (transpose n)))
     (map
      (lambda (row)
        (map (lambda (col) (dot-product row col))
             cols))
      m)))
 ]

@section[#:tag "c2e38"]{Exercise 2.38}

@examples[
 #:label #f #:eval ev #:hidden
 (define fold-right accumulate)
 ]

@examples[
 #:label #f #:eval ev
 (fold-right / 1 (list 1 2 3))
 ]

First @tt{(/ 3 2)} is computed, then this is divided by @tt{1}, making
@tt{3/2} the final answer.

@examples[
 #:label #f #:eval ev
 (define (fold-left op initial sequence)
   (define (iter result rest)
     (if (null? rest)
         result
         (iter (op result (car rest))
               (cdr rest))))
   (iter initial sequence))

 (fold-left / 1 (list 1 2 3))
 ]

However, here @tt{1} is divided by @tt{2} first, and then this is divided
by @tt{3}, making @tt{1/6} the final answer.

@examples[
 #:label #f #:eval ev
 (fold-right list nil (list 1 2 3))
 ]

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

@examples[
 #:label #f #:eval ev #:no-prompt
 (lambda (x y) (append y (list x)))
 ]

@tt{y} contains the list so far (with an initial value of @tt{nil}), while
@tt{x} is the current element being folded over. The current element gets added
to the end of the list, and since the values in the original list get added
in reverse order (due to the way the recursive process unfolds), this produces
a reversed list.

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (reverse sequence)
   (fold-right (lambda (x y) (append y (list x))) nil sequence))
 ]

Writing @tt{reverse} in terms of @tt{fold-left} is almost identical, with an
inner procedure of

@examples[
 #:label #f #:eval ev #:no-prompt
 (lambda (x y) (append (list y) x))
 ]

The two arguments of a procedure passed to @tt{fold-left} are reversed compared to
those for @tt{fold-right}. So now, @tt{y} is the current element being visited.
Since @tt{fold-left} evolves an iterative process where the elements of the list
being reversed are operated on in forward order, appending the values to the front
of the list as they are seen produces a list in reversed order.

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (reverse-foldl sequence)
   (fold-left (lambda (x y) (append (list y) x)) nil sequence))
 ]

@section[#:tag "c2e40"]{Exercise 2.40}

This procedure is almost identical to the one embedded in the original
@tt{prime-sum-pairs} procedure, with one small change: The outer map (over
values of @tt{i}) now enumerates from @tt{2} to @tt{n}, not from @tt{1}.  The
inner @tt{enumerate-interval} for @tt{j} would produce an empty list (since it
would go from @tt{1} to @tt{0}), which would get ignored when @tt{flatmap}
@tt{append}s the results together (@tt{(append nil nil)} is still @tt{nil}). A
clever trick knowing how @tt{flatmap} works, but I prefer it this way. The
definition of the unique pairs given in the book states that @tt{i > 1} anyway.

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (unique-pairs n)
   (map
    (lambda (i)
      (map (lambda (j) (list i j))
           (enumerate-interval 1 (- i 1))))
    (enumerate-interval 2 n)))
 ]

@tt{prime-sum-pairs} now looks like this:

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (prime-sum-pairs n)
   (map make-pair-sum
        (filter prime-sum? (unique-pairs n))))
 ]

@section[#:tag "c2e41"]{Exercise 2.41}

Outside the main procedure I defined a general procedure
@tt{make-ordered-triples} for generating ordered triples with positive integer
values up to @tt{n}:

@examples[
 #:label #f #:eval ev #:no-prompt
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
 ]

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

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (ordered-distinct-triples-sum n s)
   (define (valid-triple? s)
     (lambda (t)
       (and
        (= s (accumulate + 0 t))
        (and (not (= (car t) (cadr t)))
             (not (= (car t) (caddr t)))
             (not (= (cadr t) (caddr t)))))))
   (filter (valid-triple? s) (make-ordered-triples n)))
 ]

Because we only have triples, it's feasible to write a predicate to test if all
of the elements are distinct manually, as we have done above.

@section[#:tag "c2e42"]{Exercise 2.42}

@bold{I consider this exercise unfinished}

First, a utility procedure for finding if any element in a sequence evaluates
to true:

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (any? s)
   (cond ((null? s) #f)
         ((car s) #t)
         (else (any? (cdr s)))))
 ]

We can use this in the @tt{safe?} procedure, testing if the new piece conflicts
with any of the other ones. This uses a @tt{conflicts?} procedure which tests if
two pieces lay in the same row or on a diagonal.

@examples[
 #:label #f #:eval ev #:no-prompt
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
 ]

The procedure @tt{on-diagonal?} has an awkward interface and is sort of
obscure. Assuming that the @tt{j} is greater than @tt{i}, it returns true if
either the element in column @tt{j} is in the space @tt{j - i} rows above the
element in row @tt{i} (meaning the piece in column @tt{j} is diagonally up and to
the right from that in @tt{i}), or if the reverse is true and the piece in column
@tt{i} is on an upper diagonal from that in column @tt{j}.

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (on-diagonal? i ri j rj)
   ;; assumption: i < j
   (let ((d (- j i)))
     (or
      ;; case 1: diagonal up
      (= rj (+ ri d))
      ;; case 2: diagonal down
      (= ri (+ rj d)))))
 ]

Simply representing the current queen position as a list with one element per queen,
it is natural that @tt{empty-board} is just an empty list and that @tt{adjoin-position}
just appends the new position to the end of an existing list:

@examples[
 #:label #f #:eval ev #:no-prompt
 (define empty-board nil)
 ]

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (adjoin-position new-row k rest-of-queens)
   (append rest-of-queens (list new-row)))
 ]

@tt{adjoin-position} takes the new column @tt{k} in the given @tt{queens} procedure.

@section[#:tag "c2e43"]{Exercise 2.43}

In the original procedure, @tt{queen-cols} is recursively called once for a
board size of @tt{k - 1}. In Louis' version, it is called @tt{k} times. Because
it produces the same result every time, Louis' version of @tt{queens} will
still work. However, these extra calculations will make the procedure much
slower. When running @tt{(queens 8)}, @tt{(queen-cols 7)} will be called @tt{8}
times. In each of these calls, @tt{(queen-cols 6)} will be called @tt{7} times,
&etc.

To estimate the difference in running time, let's make a few simplifications to
get upper bounds on the amount of time that these procedures can take. Suppose
that every recursive call to @tt{queen-cols} except the base case takes the
same time as a call to @tt{(queen-cols 8)}, since none are any worse than this,
and that we call this time @tt{S}. There are then @tt{8} calls to @tt{queen-cols}
in the original procedure, leaving a runtime of approximately @tt{8S}.

Noting that every non-leaf node in the recursion tree of Louis' procedure has no
more than @tt{8} children, we can find an upper bound on this by supposing that
they all have exactly @tt{8}. We then have something on the order of
@tt{S + 8(S + 8(S + 8(...)))} time, or approximately @tt{(8^8)S}.

@; Evaluator for painting
@define[paint-ev @make-eval[]]
@examples[
 #:eval paint-ev #:hidden
 (#%require sicp-pict)
 ]

@section[#:tag "c2e44"]{Exercise 2.44}

@tt{up-split} is almost identical to @tt{right-split}. Knowing how @tt{below}'s
arguments work, we can use it properly to place the smaller split painters
above the original as specified:

@examples[
 #:label #f #:eval paint-ev #:no-prompt
 (define (up-split painter n)
   (if (= n 0)
       painter
       (let ((smaller (up-split painter (- n 1))))
         (below painter (beside smaller smaller)))))
 ]

@image["img/up-split.png"]

@section[#:tag "c2e45"]{Exercise 2.45}

Writing @tt{up-split}, it should have been apparent that it was almost identical to
@tt{right-split}. Now we generalize it into a general @tt{split} procedure:

@examples[
 #:label #f #:eval paint-ev #:no-prompt
 (define (split macro-op micro-op)
   (define (apply-split painter n)
     (if (= n 0)
         painter
         (let ((smaller (apply-split painter (- n 1))))
           (macro-op painter (micro-op smaller smaller)))))
   apply-split)
 ]

We cannot use a normal @tt{lambda} because the procedure we return has to call itself.
I chose to use an internal @tt{define} to create the procedure and to return it without
calling it after.

@section[#:tag "c2e46"]{Exercise 2.46}

Vectors can simply be another pair joined by @tt{cons}:

@examples[
 #:label #f #:eval paint-ev #:no-prompt
 (define (make-vect x y) (cons x y))

 (define (xcor-vect v) (car v))

 (define (ycor-vect v) (cdr v))
 ]

The operators @tt{add-vect}, @tt{sub-vect}, and @tt{scale-vect} are similarly trivial:

@examples[
 #:label #f #:eval paint-ev #:no-prompt
 (define (add-vect v1 v2)
   (make-vect (+ (xcor-vect v1) (xcor-vect v2))
              (+ (ycor-vect v1) (ycor-vect v2))))

 (define (sub-vect v1 v2)
   (make-vect (- (xcor-vect v1) (xcor-vect v2))
              (- (ycor-vect v1) (ycor-vect v2))))

 (define (scale-vect s v)
   (make-vect (* s (xcor-vect v))
              (* s (ycor-vect v))))
 ]

@section[#:tag "c2e47"]{Exercise 2.47}

If frames are defined using a list:

@examples[
 #:label #f #:eval paint-ev #:no-prompt
 (define (origin-frame frame) (car frame))

 (define (edge1-frame frame) (cadr frame))

 (define (edge2-frame frame) (caddr frame))
 ]

If frames are defined by @tt{cons}ing an origin vector onto a pair of edge
vectors:

@examples[
 #:label #f #:eval paint-ev #:no-prompt
 (define (origin-frame frame) (car frame))

 (define (edge1-frame frame) (cadr frame))

 (define (edge2-frame frame) (cddr frame))
 ]

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

@examples[
 #:label #f #:eval paint-ev #:no-prompt
 (define (make-segment start end) (cons start end))

 (define (start-segment segment) (car segment))

 (define (end-segment segment) (cdr segment))
 ]

@section[#:tag "c2e49"]{Exercise 2.49}

These procedures are victims to creeping indentation due to the nested
@tt{let}s, but we don't have the tools to do better at the moment.

As a note: The painter considers the upper bound of @tt{1.0} to not be
inside the image -- that is, points at an X or Y coordinate of @tt{1.0}
will not be drawn. To get the semantics we want, we define an @tt{upper-bound}
that can be drawn:

@examples[
 #:label #f #:eval paint-ev #:no-prompt
 (define upper-bound 0.995)
 ]

Since we're reusing the four corner points, we'll define them separately:
@examples[
 #:label #f #:eval paint-ev #:no-prompt
 (define bottom-left (make-vect 0.0 0.0))
 (define bottom-right (make-vect upper-bound 0.0))
 (define top-left (make-vect 0.0 upper-bound))
 (define top-right (make-vect upper-bound upper-bound))
 ]

Next, an outline painter:

@examples[
 #:label #f #:eval paint-ev #:no-prompt
 (define outline-painter
   (let ((bottom (make-segment bottom-left bottom-right))
         (left (make-segment bottom-left top-left))
         (right (make-segment bottom-right top-right))
         (top (make-segment top-left top-right)))
     (segments->painter
      (list bottom left right top))))
 ]

@image["img/outline.png"]

Next, an "X" painter:

@examples[
 #:label #f #:eval paint-ev #:no-prompt
 (define x-painter
   (segments->painter
    (list
     (make-segment bottom-left top-right)
     (make-segment top-left bottom-right))))
 ]

@image["img/x.png"]

These two procedures could be generalized to be applications of a procedure
creating painters working on the corners of a frame.

A painter for a diamond whose corners are the midpoints of the frame (using
hardcoded midpoints because we are working on the unit frame):

@examples[
 #:label #f #:eval paint-ev #:no-prompt
 (define diamond-painter
   (let ((bottom (make-vect 0.5 0.0))
         (left (make-vect 0.0 0.5))
         (right (make-vect upper-bound 0.5))
         (top (make-vect 0.5 upper-bound)))
     (let ((bottom-left (make-segment bottom left))
           (left-top (make-segment left top))
           (top-right (make-segment top right))
           (right-bottom (make-segment right bottom)))
       (segments->painter
        (list bottom-left left-top top-right right-bottom)))))
 ]

@image["img/diamond.png"]

Defining @tt{wave} is simply a matter of supplying @tt{segments->painter} with
a more complex list of segments. I don't think the amount to be learned here is
worth the work it takes to find these edges based on the images in the book.

@section[#:tag "c2e50"]{Exercise 2.50}

The first, @tt{flip-horiz}, is defined similarly to @tt{flip-vert}, with the new origin
in the bottom-right corner and the edges coming left and up from it:

@examples[
 #:label #f #:eval paint-ev #:no-prompt
 (define (flip-horiz painter)
   (transform-painter painter
                      (make-vect 1 0)
                      (make-vect 0 0)
                      (make-vect 1 1)))
 ]

The rest are defined in terms of painter transformations we already have:

@examples[
 #:label #f #:eval paint-ev #:no-prompt
 (define (rotate180 painter)
   (flip-vert painter))
 ]

@examples[
 #:label #f #:eval paint-ev #:no-prompt
 (define (rotate270 painter)
   (flip-horiz (rotate90 painter)))
 ]

@section[#:tag "c2e51"]{Exercise 2.51}

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

@examples[
 #:label #f #:eval paint-ev #:no-prompt
 (define (below painter-bottom painter-top)
   (let ((bottom-left (make-vect 0.0 0.0))
         (bottom-right (make-vect 1.0 0.0))
         (top-left (make-vect 0.0 1.0))
         (mid-left (scale-vect 0.5 top-left)))
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
 ]

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

@examples[
 #:label #f #:eval paint-ev #:no-prompt
 (define (below bottom top)
   (rotate90
    (beside (rotate270 bottom) (rotate270 top))))
 ]

@section[#:tag "c2e52"]{Exercise 2.52}

I did not make the @tt{wave} painter in @secref{c2e49}, so I will not be adding
segments to it.

To modify @tt{corner-split}, I will apply a @tt{flip-horiz} to the base case
and to the bottom-left painter:

@examples[
 #:label #f #:eval paint-ev #:no-prompt
 (define (corner-split painter n)
   (if (= n 0)
       (flip-horiz painter)
       (let ((up (up-split painter (- n 1)))
             (right (right-split painter (- n 1))))
         (let ((top-left (beside up up))
               (bottom-right (below up right))
               (corner (corner-split painter (- n 1))))
           (beside (below (flip-horiz painter) top-left)
                   (below bottom-right corner))))))
 ]

To modify @tt{square-limit}, I will make the patterns face outward by swapping
the left and right halves in the @tt{square-of-four} call:

@examples[
 #:label #f #:eval paint-ev #:no-prompt
 (define (square-limit painter n)
   (let ((combine4 (square-of-four identity flip-horiz
                                   flip-vert rotate180)))
     (combine4 (corner-split painter n))))
 ]

@image["img/square-limit.png"]

@section[#:tag "c2e53"]{Exercise 2.53}

@examples[
 #:label #f #:eval ev
 (list 'a 'b 'c)

 (list (list 'george))

 (cdr '((x1 x2) (y1 y2)))

 (cadr '((x1 x2) (y1 y2)))

 (pair? (car '(a short list)))

 (memq 'red '((red shoes) (blue socks)))

 (memq 'red '(red shoes blue socks))
 ]

@section[#:tag "c2e54"]{Exercise 2.54}

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (equal? a b)
   (cond
     ((and (null? a) (null? b)) #t)
     ((and (symbol? a) (symbol? b))
      (eq? a b))
     ((and (list? a) (list? b))
      (if (or (null? a) (null? b)) #f
          (and
           (equal? (car a) (car b))
           (equal? (cdr a) (cdr b)))))
     (else #f)))
 ]

@section[#:tag "c2e55"]{Exercise 2.55}

When evaluating

@racketblock[
 (car ''abracadabra)
]

the expanded evaluation is

@racketblock[
 (car (quote (quote abracadabra)))
]

It is clear from this that @tt{quote} is the corect answer.

@section[#:tag "c2e56"]{Exercise 2.56}

The definitions of @tt{exponentiation?}, @tt{base}, and @tt{exponent} are
trivial, basically identical to similar procedures for other operations:

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (exponentiation? x)
   (and (pair? x) (eq? (car x) '**)))

 (define (base x) (cadr x))

 (define (exponent x) (caddr x))
 ]

@tt{make-exponentiation} also follows the template of similar procedures for
sums and products. In addition to the base cases for exponents of @tt{0} and
@tt{1} that we were asked to do, I have handled the cases for bases of @tt{0}
and @tt{1}, as well as the case of a constant raised to a constant power.

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (make-exponentiation base exponent)
   (cond ((=number? exponent 0) 1)
         ((=number? exponent 1) base)
         ((=number? base 0) 0)
         ((=number? base 1) 1)
         ((and (number? base) (number? exponent))
          (expt base exponent))
         (else (list '** base exponent))))
 ]

However, the rule we are implementing only refers to expressions being raised
to constant powers. We could check that the exponent is a number in
@tt{make-exponentiation}, but this would be improper for expressions with
non-constant-power exponents that we might want to differentiate later. So,
I've created another procedure, @tt{constant-exponentiation?}, that checks if
an expression is an exponentiation with a constant exponent. This procedure is
used inside @tt{deriv} to implement the power rule. A simple implementation is
as follows:

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (constant-exponentiation? x)
   (and (exponentiation? x) (number? (exponent x))))
 ]

@bold{TODO: non-simple implementation}

Finally, the extended @tt{deriv} is as follows:

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (variable? x) (symbol? x))
 (define (same-variable? v1 v2)
   (and (variable? v1) (variable? v2) (eq? v1 v2)))
 (define (=number? exp num)
   (and (number? exp) (= exp num)))
 (define (make-sum a1 a2)
   (cond ((=number? a1 0) a2)
         ((=number? a2 0) a1)
         ((and (number? a1) (number? a2)) (+ a1 a2))
         (else (list '+ a1 a2))))
 (define (make-product m1 m2)
   (cond ((or (=number? m1 0) (=number? m2 0)) 0)
         ((=number? m1 1) m2)
         ((=number? m2 1) m1)
         ((and (number? m1) (number? m2)) (* m1 m2))
         (else (list '* m1 m2))))
 (define (sum? x)
   (and (pair? x) (eq? (car x) '+)))
 (define (addend s) (cadr s))
 (define (augend s) (caddr s))
 (define (product? x)
   (and (pair? x) (eq? (car x) '*)))
 (define (multiplier p) (cadr p))
 (define (multiplicand p) (caddr p))

 (define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp)
          (if (same-variable? exp var) 1 0))
         ((sum? exp)
          (make-sum (deriv (addend exp) var)
                    (deriv (augend exp) var)))
         ((product? exp)
          (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
         ((constant-exponentiation? exp)
          (make-product
           (make-product
            (exponent exp)
            (make-exponentiation (base exp)
                                 (- (exponent exp) 1)))
           (deriv (base exp) var)))
         (else
          (error "unknown expression type -- DERIV" exp))))
 ]

@section[#:tag "c2e57"]{Exercise 2.57}

It is enough to change @tt{augend} (and @tt{multiplicand}) to return the sum
(and product) of the terms after the @tt{addend} (or @tt{multiplier}). However,
we cannot always append a @tt{+} or @tt{*} to the front of the rest of the
terms, or we will end up with things like @tt{(+ x)} when there is only one
term left. Therefore, we must only return the last term (the behavior we
already have) if there is one term left, and otherwise return a new sum (or
product). We do not have to change @tt{deriv} at all. The implementations are
below.

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (augend s)
   (if (null? (cdddr s))
       (caddr s)
       (append (list '+) (cddr s))))

 (define (multiplicand p)
   (if (null? (cdddr p))
       (caddr p)
       (append (list '*) (cddr p))))
 ]

We can verify that this works by taking the book's cue:

@examples[
 #:label #f #:eval ev
 (equal?
  (deriv '(* (* x y) (+ x 3)) 'x)
  (deriv '(* x y (+ x 3)) 'x))
 ]

@section[#:tag "c2e58"]{Exercise 2.58}

Changing to fully-parenthesized prefix notation is simple: In
operation-selection procedures for the first argument (e.g. @tt{addend}), use
@tt{car}.

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (addend s) (car s))

 (define (multiplier p) (car p))

 (define (base x) (car x))
 ]

Then, inside the operation-construction procedures (e.g. @tt{make-sum}), swap
the positions of the operator and the first argument in the list.

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (make-sum a1 a2)
   (cond ((=number? a1 0) a2)
         ((=number? a2 0) a1)
         ((and (number? a1) (number? a2)) (+ a1 a2))
         (else (list a1 '+ a2))))

 (define (make-product m1 m2)
   (cond ((or (=number? m1 0) (=number? m2 0)) 0)
         ((=number? m1 1) m2)
         ((=number? m2 1) m1)
         ((and (number? m1) (number? m2)) (* m1 m2))
         (else (list m1 '* m2))))

 (define (make-exponentiation base exponent)
   (cond ((=number? exponent 0) 1)
         ((=number? exponent 1) base)
         ((=number? base 0) 0)
         ((=number? base 1) 1)
         ((and (number? base) (number? exponent))
          (expt base exponent))
         (else (list base '** exponent))))
 ]

Then, inside all of the operation-detection procedures (e.g. @tt{sum?}), use
@tt{cadr}. You could also generalize this to an @tt{operator} procedure, but I
won't.

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (sum? x)
   (and (pair? x) (eq? (cadr x) '+)))

 (define (product? x)
   (and (pair? x) (eq? (cadr x) '*)))

 (define (exponentiation? x)
   (and (pair? x) (eq? (cadr x) '**)))
 ]

To support a more conventional notation with omitted parentheses and an order
of operations, we need to parse flat structures into more hierarchically
structured forms. One way to think about the fully-parenthesized forms we've
been dealing with now is as intermediate forms between the natural notation and
the computed result. This is essentially the same idea that is behind Lisp's
syntax being representative of the abstract syntax tree.

For example, if we have the expression

@verbatim{
 3 + x * 5
}

we want to turn it into

@verbatim{
 3 + (x * 5)
}

But is this form a product or a sum? Or is it both? We can use the rules of
differentiation to decide on a good answer. It is clear that @tt{d/dx 3+x*5} is
the same as @tt{d/dx 3 + d/dx x*5}, @tt{5}. However, this is not the same as

@verbatim{
 (3 + x) * (d/dx 5) + (d/dx 3 + x) * 5
 = x + 8
}

Since the rule for sums applies to the expression but the rule for products
does not, it is reasonable to say that the expression is a sum and not a
product. Intuitively, this is because an expression is a product only if the
last operation performed is multiplication. In other words, the type of the
expression is the operation in the expression with the least precedence in our
order of operations.

To make this and a later task easier, we'll first define a procedure for finding
all of the items before and after a specific element in a list. If the element is
not in the list, we will return false. If there are no elements in a group, we will
return an empty list for that group.

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (find-groups item x)
   (define (group-iter item before after)
     (cond ((null? after) #f)
           ((eq? item (car after))
            (cons before (cdr after)))
           (else
            (group-iter item
                        (append before (list (car after)))
                        (cdr after)))))
   (group-iter item '() x))
 ]

Since summing is the least precedent operation, @tt{sum?} is easy to define. We
can use @tt{find-groups} to determine if @tt{+} is in the expression and if
there are elements before and after it.

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (sum? x)
   (let ((groups (find-groups '+ x)))
     (if groups
         (and (not (null? (car groups)))
              (not (null? (cdr groups))))
         #f)))
 ]

To implement @tt{product?}, since addition is the operation below
multiplication in precedence, we can return true if the expression is a valid
multiplication and if the expression is not a sum.

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (product? x)
   (let ((groups (find-groups '* x)))
     (if groups
         (and (not (null? (car groups)))
              (not (null? (cdr groups)))
              (not (sum? x)))
         #f)))
 ]

However, this is obviously a good case for an abstraction. Since the order
requirement does not depend on the group requirements, we can create a more
general @tt{valid-operation?} procedure and reimplement @tt{sum?} and
@tt{product?} in terms of it:

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (valid-operation? op x)
   (let ((groups (find-groups op x)))
     (if groups
         (and (not (null? (car groups)))
              (not (null? (cdr groups))))
         #f)))

 (define (sum? x)
   (valid-operation? '+ x))

 (define (product? x)
   (and (valid-operation? '* x)
        (not (sum? x))))
 ]

Exponentiation has precedence over multiplication, so we can implement this
check by making sure that an expression is neither a product nor a sum,
in addition to being a valid exponentiation.

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (exponentiation? x)
   (and (valid-operation? '** x)
        (not (product? x))
        (not (sum? x))))
 ]

Alternatively, instead of checking the order of operations inside of these
procedures, you could define it in the general evaluator -- in this case,
@tt{deriv}. If you tested them in order of ascending precedence, you would
know, for example, if an expression was being tested as a product, it was
definitely not a sum. When dealing with more operations, this becomes a better
solutions -- you can avoid a good amount of retesting and make your
operation-testing procedures simpler. This is what we will be doing, leaving
the final procedures as such:

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (sum? x)
   (valid-operation? '+ x))

 (define (product? x)
   (valid-operation? '* x))

 (define (exponentiation? x)
   (valid-operation? '** x))
 ]

To define the operand-selecting procedures, we can use the information from
@tt{find-groups}. Since this procedure just returns a pair, the new work we
have to do is minimal.

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (addend s) (car (find-groups '+ s)))

 (define (augend s) (cdr (find-groups '+ s)))

 (define (multiplier p) (car (find-groups '* p)))

 (define (multiplicand p) (cdr (find-groups '* p)))

 (define (base x) (car (find-groups '** x)))

 (define (exponent x) (cdr (find-groups '** x)))
 ]

Our procedures for constructing sums, products, and exponentiations still
return fully-parenthesized results. This does not need to be fixed. However, we
do need to change how variables and numbers are handled, now that both operands
are lists by default. An easy way to do this is to add another case to @tt{deriv}
for lists of a single value. This leaves @tt{deriv} (already with the correct order
of operations, by chance) as such:

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp)
          (if (same-variable? exp var) 1 0))
         ((and (list? exp) (null? (cdr exp)))
          (deriv (car exp) var))
         ((sum? exp)
          (make-sum (deriv (addend exp) var)
                    (deriv (augend exp) var)))
         ((product? exp)
          (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
         ((constant-exponentiation? exp)
          (make-product
           (make-product
            (exponent exp)
            (make-exponentiation (base exp)
                                 (- (exponent exp) 1)))
           (deriv (base exp) var)))
         (else
          (error "unknown expression type -- DERIV" exp))))
 ]

As one small point, we also must change @tt{constant-exponentiation?} to expect
the exponent to be in a list:

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (constant-exponentiation? x)
   (and (exponentiation? x) (number? (car (exponent x)))))
 ]

And with this, we're done.

@section[#:tag "c2e59"]{Exercise 2.59}

@tt{union-set} is very similar to @tt{intersection-set}, with two key differences:

@itemlist[
 @item{If an element of the first set is already in the second, it is not added to
  the result, and vice versa. This is the opposite of how intersections work.}
 @item{If either of the sets is empty, the other set is returned, not the empty set.}
 ]

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (union-set set1 set2)
   (cond ((null? set1) set2)
         ((null? set2) set1)
         ((element-of-set? (car set1) set2)
          (union-set (cdr set1) set2))
         (else (cons (car set1)
                     (union-set (cdr set1) set2)))))
 ]

@section[#:tag "c2e60"]{Exercise 2.60}

When sets are represented as lists allowing duplicates, @tt{element-of-set?}
and @tt{intsersection-set} can be identical. And in @tt{adjoin-set} and
@tt{union-set}, we can remove anything having to do with a call to
@tt{element-of-set?} because we no longer care about making sure elements are
unique.

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (adjoin-set x set)
   (cons x set))
 ]

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (union-set set1 set2)
   (append set1 set2))
 ]

Notice how @tt{union-set} is no longer defined recursively -- it simply uses
the general function @tt{append} to do all of the work.

Naturally, this set implementation will be slower, possibly much slower, when
@tt{element-of-set?} is called, just because the size of the set representation
is no longer constrained by the number of unique elements in it. This in turn
means that @tt{intersection-set} is also slower. However, @tt{adjoin-set} and
@tt{union-set} are going to be faster with this representation, because they no
longer need to examine sets before operating on them.

If your application adds to sets much more frequently than it examines them, it
might be faster overall to use this representation, especially if it mostly adds
unique elements anyway.

@section[#:tag "c2e61"]{Exercise 2.61}

The new @tt{adjoin-set} does not examine the given set at all if the element is
smaller than or equal to its first element. In these cases, we know that the
resulting set is either the @tt{cons} of the element and the set or just the
set, respectively. If the element is larger than the first element of the set,
then it does need to traverse to the next element until one of the cases
above is true (or until the set is empty, in which case a new set just containing
the element is returned).

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (adjoin-set x set)
   (if (null? set) (list x)
       (let ((y (car set)))
         (cond ((= x y) set)
               ((< x y) (cons x set))
               (else (cons y (adjoin-set x (cdr set))))))))
 ]

Note that we need to have a @tt{null?} check in @tt{adjoin-set} now, because of
the operations requiring that set to not be empty. Before, when @tt{cons}ing
the element and the set, it didn't matter if the set was empty -- and
@tt{element-of-set?|} didn't care either.

Since we can expect that, on average, elements added to a set will be in the
middle of the set, we can expect about half as many recursive calls to
@tt{adjoin-set} as we did previously.

@section[#:tag "c2e62"]{Exercise 2.62}

Like the new @tt{intersection-set}, this implementation of @tt{union-set} is @tt{O(n)}
because we examine the elements of each set one time at most.

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (union-set set1 set2)
   (cond ((null? set1) set2)
         ((null? set2) set1)
         (else
          (let ((x1 (car set1)) (x2 (car set2)))
            (cond ((= x1 x2)
                   (cons x1 (union-set (cdr set1) (cdr set2))))
                  ((< x1 x2)
                   (cons x1 (union-set (cdr set1) set2)))
                  ((< x2 x1)
                   (cons x2 (union-set set1 (cdr set2)))))))))
 ]

@section[#:tag "c2e63"]{Exercise 2.63}

We're comparing the following procedures for converting binaries trees into
lists:

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (tree->list-1 tree)
   (if (null? tree)
       '()
       (append (tree->list-1 (left-branch tree))
               (cons (entry tree)
                     (tree->list-1 (right-branch tree))))))
 ]

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (tree->list-2 tree)
   (define (copy-to-list tree result-list)
     (if (null? tree) result-list
         (copy-to-list (left-branch tree)
                       (cons (entry tree)
                             (copy-to-list (right-branch tree)
                                           result-list)))))
   (copy-to-list tree '()))
 ]

The first procedure @tt{append}s the list representation of the
@tt{left-branch} of the tree to the list formed by @tt{cons}ing the @tt{entry}
of the tree with the list representation of the @tt{right-branch} of the
tree. This produces a list with elements sorted in ascending order from left to
right. (The base case, where the tree is empty, is to return an empty list.)

The second procedure defines an inner procedure, @tt{copy-to-list}, taking a
tree and a list as parameters. The base case of an empty tree is the same;
however, rather than returning an empty list, the given @tt{result-list}
argument is returned. Otherwise, it calls @tt{copy-to-list}, with the first
argument being the @tt{left-branch} of the tree and the second being the list
formed by @tt{cons}ing the @tt{entry} of the tree and the list representation
of the @tt{right-branch} of the tree. To create the second argument,
@tt{copy-to-list} is called with the @tt{right-branch} of the tree and the
@tt{result-list} from the outer @tt{copy-to-list} scope supplied as arguments.

The arguments to @tt{cons} suggest that the resulting list will also be in
ascending order. The two procedures visit the nodes in the same order -- they
just accumulate their answers in a different way. The first procedure is
arguably more straightforward, by just @tt{append}ing results of recursive
calls in a predictable order. The second procedure, however, first accumulates
new results in the innermost @tt{copy-to-list} call, and then uses that with
@tt{cons} to add the value of the current node, and then uses this with an
outer @tt{copy-to-list} as a result-list when working through the left side of
the tree. The end result is the same, but the pipelining of data is more
explicit.

We can use the trees in from figure 2.16 to verify that these procedures
produce the same results:

@;examples[#:eval ev
@codeblock{
 (define ta (list 7 (list 3 (list 1 '() '()) (list 5 '() '())) (list 9 '() (list 11 '() '()))))
 (define tb (list 3 (list 1 '() '()) (list 7 (list 5 '() '()) (list 9 '() (list 11 '() '())))))
 (define tc (list 5 (list 3 (list 1 '() '()) '()) (list 9 (list 7 '() '()) (list 11 '() '()))))

 (tree->list-1 ta)

 (tree->list-2 ta)

 (tree->list-1 tb)

 (tree->list-2 tb)

 (tree->list-1 tc)

 (tree->list-2 tc)
}

However, although these procedures produce the same results, they do not have
the same order of growth. The second procedure, visiting every node in the tree
once and applying @tt{cons} once per visit, is clearly @tt{O(N)}.

However, the first procedure calls @tt{append} when visiting each node. Recall
from earlier in the chapter that @tt{append} is an @tt{O(N)} operation. We can
formalize the running time of the first procedure as a recurrence relation:

@verbatim{
 T(n) = O(n) + 2T(n/2)
}

This has a solution of @tt{O(nlogn)}.

@section[#:tag "c2e64"]{Exercise 2.64}

@bold{NOTE: This is not quite going to be in the form of a "short paragraph". I
 hope you understand.}

We are asked to explain how the following procedure @tt{partial-tree}
works:

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (partial-tree elts n)
   (if (= n 0)
       (cons '() elts)
       (let ((left-size (quotient (- n 1) 2)))
         (let ((left-result (partial-tree elts left-size)))
           (let ((left-tree (car left-result))
                 (non-left-elts (cdr left-result))
                 (right-size (- n (+ left-size 1))))
             (let ((this-entry (car non-left-elts))
                   (right-result (partial-tree (cdr non-left-elts)
                                               right-size)))
               (let ((right-tree (car right-result))
                     (remaining-elts (cdr right-result)))
                 (cons (make-tree this-entry left-tree right-tree)
                       remaining-elts))))))))
 ]

Before beginning, I would first reformat the procedure using a form we haven't
learned yet, @tt{let*}, which allows for earlier bindings available to later
ones. This removes the need for the creeping indentation we see above.

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (partial-tree elts n)
   (if (= n 0)
       (cons '() elts)
       (let* ((left-size (quotient (- n 1) 2))
              (left-result (partial-tree elts left-size))
              (left-tree (car left-result))
              (non-left-elts (cdr left-result))
              (right-size (- n (+ left-size 1)))
              (this-entry (car non-left-elts))
              (right-result (partial-tree (cdr non-left-elts) right-size))
              (right-tree (car right-result))
              (remaining-elts (cdr right-result)))
         (cons (make-tree this-entry left-tree right-tree) remaining-elts))))
 ]

This procedure is more involved than most we've worked with so far, but if we
break down what every definition is computing, we can piece together what the
procedure is doing fairly easily. We'll do it line-by-line.

@verbatim{
 (if (= n 0)
 ...)
}

This sets up the base case of the procedure, which is clearly going to be
recursive.  We can look at the way @tt{partial-tree} is called to understand
what the base case means:

@racketblock[
 (define (list->tree elements)
 (car (partial-tree elements (length elements))))
]

So @tt{n} is the number of elements that this call of @tt{partial-tree} is going to
process. If there are none left, then we evaluate

@racketblock[
 (cons '() elts)
]

and return an empty list (meaning an empty tree -- there are no elements left
to add) paired with the @tt{elts} we passed to @tt{partial-tree}.

@verbatim{
 (let* ((left-size (quotient (- n 1) 2))
 ...))
}

This sets up the general case. It exists inside a @tt{let*} form (or the first
of quite a few regular @tt{let}s in the original), giving us a scope to define
values that we will use to get our result. The first of these is @tt{left-size},
which is @tt{(n - 1) / 2}. We'll see why we subtract @tt{1} later; for now, just
understand that this is our value called @tt{left-size}.

@racketblock[
 (left-result (partial-tree elts left-size))
]

The value @tt{left-result} is given the meaning of the tree representation of
@tt{left-size} elements from @tt{elts}. Instead of creating a sublist with just
this many elements, we use the @tt{n} argument in @tt{partial-tree} to regulate
how many elements are taken in all of the recursive calls (as seen here --
we're only turning the first half of the list into a tree).

@racketblock[
 (left-tree (car left-result))
 (non-left-elts (cdr left-result))
]

We know that @tt{partial-tree}, in the base case, returns a pair consisting of
an empty list and the elements passed to it that weren't used in that base
case.  This confirms that, in general, the @tt{car} of the pair is the
generated tree (which in the base case is empty) and the @tt{cdr} is the
remaining elements (which in the base case is all of them).

Can we be sure that @tt{non-left-elts} will contain the elements not used in
the left side of the tree? We can start to imagine why this might be correct
with a case where @tt{n} is equal to @tt{1}. Clearly, @tt{left-size} will be
@tt{0}, making @tt{left-result} an empty list, @tt{left-tree} an empty tree,
and @tt{non-left-elts} contain all of our original elements, none of which are
in @tt{left-tree}. The reasons for this behavior will become clearer in a
moment, but it is at least plausible that this works.

@racketblock[
 (right-size (- n (+ left-size 1)))
]

This is similar to @tt{left-size}, and is probably the number of elements that
are going to be in the right-side tree. And again, we see that the size is not
equal to @tt{n} minus @tt{left-size}, but is one less than that. In other words,
@tt{left-size} and @tt{right-size}, moving inward, leave one element in the middle of
the list. And what is this used for? We can see in the next line:

@racketblock[
 (this-entry (car non-left-elts))
]

We assign the first element in @tt{non-left-elts} to a new value,
@tt{this-entry}. It is reasonable to suspect that this is going to be the root
of the tree. This makes it obvious why the sizes of the left and right branches
leave space for one more element -- of course the root element of a tree can't
be in either of its subtrees. This is what happens to the first element of
@tt{elts} in the case considered above, where @tt{n} is @tt{1} -- exactly as we
would expect.

@racketblock[
 (right-result (partial-tree (cdr non-left-elts) right-size))
]

This computes the right side of the tree, using @tt{right-size} and the elements of
@tt{non-left-elts} following the root of the tree.

@racketblock[
 (right-tree (car right-result))
 (remaining-elts (cdr right-result))
]

These correspond to the definitions earlier, getting the computed tree and the elements
not used it in from the pair produced by @tt{partial-tree}.

Having now processed all of the bindings in the @tt{let*} expression, we only have
the body of the procedure left:

@racketblock[
 (cons (make-tree this-entry left-tree right-tree) remaining-elts)
]

This is just the general case of the result we saw earlier in the base case, and have
used twice in the procedure so far. By this time, it should be clear how it works.

Evaluating @tt{list->tree} on the given list, we get the following tree:

@verbatim{
 (5
 (1 ()
 (3 () ()))
 (9 (7 () ())
 (11 () ())))
}

Or, in a particularly bad graphical style,

@verbatim{
 5
 |- 1
 |  |- 3
 |  |-
 |
 |- 9
 |- 7
 |- 11
}

@tt{***}

Now that we understand how the procedure works, what is the order of growth in
time complexity? Actually, it's rather easy. Even though we have recursive
calls to @tt{partial-tree}, we are careful to only visit every element of the
list once. Once elements are added to a tree, they are never visited again, and
elements that haven't been added to the tree yet are not visited until they are
explicitly added to it. And elements are added to the tree with an @tt{O(1)}
@tt{cons}. Therefore, the procedure has an order of growth in execution time of
@tt{O(N)}.

@section[#:tag "c2e65"]{Exercise 2.65}

In exercises @secref{c2e63} and @secref{c2e64} we found @tt{O(N)} algorithms
for converting between ordered lists and balanced binary trees. We already have
@tt{O(N)} algorithms for computing the intersections and unions of sets
represented as ordered lists. Therefore, we can find the intersections and
unions of sets represented as binary trees in @tt{O(N)} time by first
converting from binary trees to lists, then finding the intersection or union
of these lists, and finally converting the result back into a tree. As the sum
of a fixed number of @tt{O(N)} processes, this process is also @tt{O(N)}.

I use @tt{let*} again for clarity. For the sake of avoiding name collisions,
I've assumed that the @tt{intersection-set} and @tt{union-set} procedures for
operating on ordered lists are actually suffixed with @tt{oset}. This is ugly,
but remember that it is ugly; we'll be seeing how to deal with this sort of
situation later.

First, I implemented the procedures in the naive way:

@examples[
 #:label #f #:eval ev #:hidden
 (define (intersection-oset set1 set2)
   (cond ((or (null? set1) (null? set2)) '())
         ((element-of-set? (car set1) set2)
          (cons (car set1)
                (intersection-oset (cdr set1) set2)))
         (else (intersection-oset (cdr set1) set2))))
 (define union-oset union-set)
 ]

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (intersection-btset set1 set2)
   (let* ((oset1 (tree->list-2 set1))
          (oset2 (tree->list-2 set2))
          (intersection (intersection-oset oset1 oset2)))
     (list->tree intersection)))
 ]

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (union-btset set1 set2)
   (let* ((oset1 (tree->list-2 set1))
          (oset2 (tree->list-2 set2))
          (union (union-oset oset1 oset2)))
     (list->tree union)))
 ]

However, this is plainly duplicative. We can extract a new procedure, here
named @tt{btset-convert-op} (a name so bad it will not conflict with anything
useful) that takes a procedure as an argument and returns a procedure taking
two sets and returning that operation applied to them:

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (btset-convert-op op)
   (lambda (set1 set2)
     (let* ((oset1 (tree->list-2 set1))
            (oset2 (tree->list-2 set2))
            (list-result (op oset1 oset2)))
       (list->tree list-result))))
 ]

Then, we can define our intersection and union procedures easily:

@examples[
 #:label #f #:eval ev #:no-prompt
 (define intersection-btset (btset-convert-op intersection-oset))

 (define union-btset (btset-convert-op union-oset))
 ]

Assuming that operations will only take two arguments, I think this is good
enough.  It could reduce the duplication of @tt{tree->list-2} with a @tt{map},
and @tt{apply}ing the operation to this, but this code is more straightforward,
and I think this is a win.

But unions and intersections of sets are @emph{not} only useful as binary
operations. There are two different ways to handle this. One is to make
@tt{btset-convert-op} take an arbitrary number of arguments, and process them
using the techniques above. However, this means that operations have to handle
arbitrary numbers of arguments as well. Another way to solve this problem, and
I believe a better way, is to @tt{accumulate} over all of the sets with the
operation.

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (btset-convert-op op)
   (lambda sets
     (let ((osets (map tree->list-2 sets)))
       (list->tree (accumulate op (car osets) (cdr osets))))))
 ]

Despite the naming conventions, I believe this is a reasonable answer.

@bold{TODO: I complain about the names. I should fix them.}

@section[#:tag "c2e66"]{Exercise 2.66}

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (lookup key set)
   (if (null? set)
       false
       (let ((e (entry set)))
         (let ((entry-key (car e))
               (entry-val (cdr e)))
           (cond ((= key entry-key) entry-val)
                 ((< key entry-key) (lookup key (left-branch set)))
                 ((> key entry-key) (lookup key (right-branch set))))))))
 ]

@section[#:tag "c2e67"]{Exercise 2.67}

@examples[
 #:label #f #:eval ev #:hidden
 (define (make-leaf symbol weight)
   (list 'leaf symbol weight))
 (define (leaf? object)
   (eq? (car object) 'leaf))
 (define (symbol-leaf x) (cadr x))
 (define (weight-leaf x) (caddr x))
 (define (make-code-tree left right)
   (list left
         right
         (append (symbols left) (symbols right))
         (+ (weight left) (weight right))))
 (define (left-branch tree) (car tree))
 (define (right-branch tree) (cadr tree))
 (define (symbols tree)
   (if (leaf? tree)
       (list (symbol-leaf tree))
       (caddr tree)))
 (define (weight tree)
   (if (leaf? tree)
       (weight-leaf tree)
       (cadddr tree)))
 (define (decode bits tree)
   (define (decode-1 bits current-branch)
     (if (null? bits)
         '()
         (let ((next-branch
                (choose-branch (car bits) current-branch)))
           (if (leaf? next-branch)
               (cons (symbol-leaf next-branch)
                     (decode-1 (cdr bits) tree))
               (decode-1 (cdr bits) next-branch)))))
   (decode-1 bits tree))
 (define (choose-branch bit branch)
   (cond ((= bit 0) (left-branch branch))
         ((= bit 1) (right-branch branch))
         (else (error "bad bit - CHOOSE-BRANCH" bit))))
 (define (adjoin-set x set)
   (cond ((null? set) (list x))
         ((< (weight x) (weight (car set))) (cons x set))
         (else (cons (car set)
                     (adjoin-set x (cdr set))))))
 (define (make-leaf-set pairs)
   (if (null? pairs)
       '()
       (let ((pair (car pairs)))
         (adjoin-set (make-leaf (car pair)
                                (cadr pair))
                     (make-leaf-set (cdr pairs))))))
 ]
To decode the given @tt{sample-message}, using the given Huffman tree,
we can simply call @tt{decode} with them as arguments:

@examples[
 #:label #f #:eval ev
 (define sample-tree
   (make-code-tree (make-leaf 'A 4)
                   (make-code-tree
                    (make-leaf 'B 2)
                    (make-code-tree (make-leaf 'D 1)
                                    (make-leaf 'C 1)))))
 (define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
 (decode sample-message sample-tree)
 ]

The message is decoded this:

@verbatim{
 0 1 1 0 0 1 0 1 0 1 1 1 0
 A     D A   B   B     C A
}

@section[#:tag "c2e68"]{Exercise 2.68}

@tt{encode-symbol} requires us to visit all of the nodes in the tree, and must
return a list of bits representing the path to the node containing the symbol
we are looking for if it is present in the tree. The exercise asks us to throw
an error if the symbol is not found. We will handle this (and only this) in an
outer procedure, while doing the traversal and message returning inside an inner
procedure, because it is rather elegant to return nil if the symbol is not found.

First, the inner procedure, named @tt{encode-symbol-1}:

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (encode-symbol-1 symbol tree bits)
   (if (leaf? tree)
       (if (eq? symbol (symbol-leaf tree))
           bits
           '())
       (append
        (encode-symbol-1 symbol (left-branch tree) (append bits '(0)))
        (encode-symbol-1 symbol (right-branch tree) (append bits '(1))))))
 ]

When @tt{encode-symbol-1} is visiting a current node (the root of @tt{tree}),
@tt{bits} contains the encoded path to this node. Therefore, if the current
node is a leaf and its symbol is the one we're looking for, we can return
@tt{bits}. If the current node is a leaf and the symbol is not the one we're
looking for, there's no more work to be done in the tree and we return nil,
for reasons that will become clearer soon.

If the current node is not a leaf, we need to search both the left and right
subtrees for the symbol. We can call @tt{encode-symbol-1} with the left and
right branches of the tree and with @tt{0} and @tt{1} appended to @tt{bits},
respectively. This encodes the path we have taken through the tree into the
list of bits.

However, despite doing two recursive searches for the symbol, we only want
to return one list of bits. The easiest way to do this is to @tt{append} the
results together, and to make sure that we return nil if we don't find the
symbol. This will make the final result be the same as the bit encoding of
the symbol in the tree.

To return an error when the symbol isn't found, we can write a simple wrapper
procedure as our actual @tt{encode-symbol}:

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (encode-symbol symbol tree)
   (let ((result (encode-symbol-1 symbol tree '())))
     (if (null? result)
         (error "Couldn't find symbol in tree")
         result)))
 ]

To verify that this works, we can add to our @tt{equal?} procedure from
@secref{c2e54} to also check equality of numbers, and then use if to verify
that the sample encoded message is the same as a message we encode ourselves
using our previously decoded message:

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (equal? a b)
   (cond
     ((and (null? a) (null? b)) #t)
     ((and (symbol? a) (symbol? b))
      (eq? a b))
     ((and (number? a) (number? b))
      (= a b))
     ((and (list? a) (list? b))
      (if (or (null? a) (null? b)) #f
          (and
           (equal? (car a) (car b))
           (equal? (cdr a) (cdr b)))))
     (else #f)))
 ]

@examples[
 #:label #f #:eval ev
 (define (encode message tree)
   (if (null? message)
       nil
       (append (encode-symbol (car message) tree)
               (encode (cdr message) tree))))
 (equal? sample-message (encode (decode sample-message sample-tree) sample-tree))
 ]

@section[#:tag "c2e69"]{Exercise 2.69}

We are asked to write the procedure @tt{successive-merge}, which will
be used in generating Huffman trees as follows:

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (generate-huffman-tree pairs)
   (successive-merge (make-leaf-set pairs)))
 ]

Most of the hard work has already been done for us in the procedures written
during this section. @tt{make-leaf-set} will order the set of pairs, so we can
take the first two elements from the set to get the two elements with the lowest
weights. During the successive merging, we will have to maintain this ordering
by placing the newly-merged trees in the correct place by their merged weights.
We can do this by using the @tt{adjoin-set} procedure that @tt{make-leaf-set}
also uses. @tt{adjoin-set} is given as its arguments the newly-merged tree,
constructed from the first two elements of the @tt{leaf-set}, and the rest
(@tt{cddr}) of the @tt{leaf-set}.

With a new leaf set constructed, we have to call @tt{successive-merge} again,
to continue merging until we're done. The base case is that @tt{leaf-set} is
a list containing the final tree as its only element. In this case, the @tt{cdr}
is @tt{nil}, and we return the tree itself.

The final procedure is below:

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (successive-merge leaf-set)
   (if (null? (cdr leaf-set))
       (car leaf-set)
       (successive-merge
        (adjoin-set
         (make-code-tree (car leaf-set) (cadr leaf-set))
         (cddr leaf-set)))))
 ]

@section[#:tag "c2e70"]{Exercise 2.70}

The first thing we need to do is generate the weights from the
text of the song. This can be done simply but inefficiently by
creating a list of symbol-count pairs that we update as we
traverse the list of symbols.

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (partition-pairs ps key)
   (define (loop front back)
     (cond ((null? back) (cons front back))
           ((eq? key (caar back)) (cons front back))
           (else (loop (cons (car back) front) (cdr back)))))
   (loop '() ps))

 (define (generate-weights symlist)
   (define (add-symbol weights symbols)
     (if (null? symbols)
         weights
         (let ((partition (partition-pairs weights (car symbols))))
           (let ((front (car partition))
                 (back (cdr partition)))
             (if (null? back)
                 (add-symbol (cons (cons (car symbols) 1) front) (cdr symbols))
                 (add-symbol (append front
                                     (cons (cons (caar back) (+ 1 (cdar back)))
                                           (cdr back)))
                             (cdr symbols)))))))
   (map (lambda (p) (list (car p) (cdr p))) (add-symbol '() symlist)))
 ]

We can set up the tree and the message as such:

@examples[
 #:label #f #:eval ev #:no-prompt
 (define song
   '(get a job
         sha na na na na na na na na
         get a job
         sha na na na na na na na na
         wah yip yip yip yip yip yip yip yip yip
         sha boom))

 (define song-tree (generate-huffman-tree (generate-weights song)))
 ]

Using the @tt{pretty-display} function (from Racket), we can see
the structure of the constructed tree:

@examples[
 #:label #f #:eval ev
 (pretty-display song-tree)
 ]

Encoding the message is then simple, and we can see that it has a length of @tt{84}:

@examples[
 #:label #f #:eval ev
 (length (encode song song-tree))
 ]

If we used a fixed-length code, we would need to use a code of 3 bits, for
@tt{2^3 = 8} symbols. Since there are

@examples[
 #:label #f #:eval ev
 (length song)
 ]

symbols in the song, we would need @tt{3 * 36 = 108} bits using our
fixed-length code.  Just like the first example in the chapter, the encoded
length with the Huffman tree is @tt{7/9}th the length of the fixed-length
encoding.

@bold{TODO: More about efficiency?}

@section[#:tag "c2e71"]{Exercise 2.71}

@bold{TODO}

@section[#:tag "c2e72"]{Exercise 2.72}

@bold{TODO}

@section[#:tag "c2e73"]{Exercise 2.73}

We are asked to consider rewriting a procedure for symbolic differentiation,
originally written like this:

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (deriv-1 exp var)
   (cond ((number? exp) 0)
         ((variable? exp) (if (same-variable? exp var) 1 0))
         ((sum? exp)
          (make-sum (deriv (addend exp) var)
                    (deriv (augend exp) var)))
         ((product? exp)
          (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
         (else (error "unknown expression type -- DERIV" exp))))
 ]

so that it uses data-directed programming, like this:

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) (if (same-variable? exp var) 1 0))
         (else ((get 'deriv (operator exp)) (operands exp)
                                            var))))

 (define (operator exp) (car exp))
 (define (operand exp) (cdr exp))
 ]

The general case is to find operators, which are represented as symbols such as
@tt{'+} and exist in pairs with their operands, and dispatch on the entries in
the @tt{deriv} table for differentiating these operations. This means that we can't
dispatch on numbers or variables, because they aren't pairs with operators and
operands.

For example, we could add the entries for taking derivatives of additions or
multiplications like this:

@codeblock{
(put 'deriv '(+)
     (lambda (exp var)
       (make-sum (deriv (addend exp) var)
                 (deriv (augend exp) var))))

(put 'deriv '(*)
     (lambda (exp var)
       (make-sum
        (make-product (multiplier exp)
                      (deriv (multiplicand exp) var))
        (make-product (deriv (multiplier exp) var)
                      (multiplicand exp)))))
}

We could also add a procedure for constant exponentiation, as per @secref{c2e56}:

@codeblock{
(put 'deriv '(^)
     (lambda (exp var)
       (make-product
        (make-product
         (exponent exp)
         (make-exponentiation (base exp)
                              (- (exponent exp) 1)))
        (deriv (base exp) var))))
}

If, however, we instead made tables with the operators as the @tt{op}s and
@tt{'deriv} as the @tt{type}, the opposite of what we do now, we would have to
register these differently, swapping the second and third arguments to @tt{put}
and changing the call to @tt{get} in @tt{deriv}.

@bold{TODO: come back later and test this}

@section[#:tag "c2e74"]{Exercise 2.74}

@bold{This exercise is powered by Magical Thinking(TM).
 It also needs to be finished.}

We assume that lookup procedures are stored in a @tt{'lookup} table
keyed by the division names.

@codeblock{
 (define (get-record division name)
 ((get 'lookup division) name))
}

We choose to pass a record to @tt{get-salary} to avoid coupling
with @tt{get-record}.

@codeblock{
 (define (get-salary division employee-record)
   ((get 'salary division) employee-record))
}

We assume that the lookup procedures return @tt{nil} if a record can't
be found, and use @tt{flatmap} to get rid of them, leaving only a valid
record if one exists.

@codeblock{
 (define (find-employee-record divisions name)
   (flatmap
    (lambda (d) (get-record d name))
    divisions))
}

If Insatiable takes over a new company, they have a few options for integrating
their separate employee records systems. One is to add another layer of
indirection by creating a new table keyed by the company. For example:

@codeblock{
 (define (get-record company division name)
   (((get 'get company) 'lookup division) name))
}

This is less invasive than possibly modifying the names of the divisions to
avoid collisions.

Looking up @tt{get} is not the only thing we could do -- we could write a new
version of @tt{get} that takes the company and the division and handles a second
layer of lookups itself. Writing a more generic version of @tt{get}, instead of
having procedures tied directly to tables (as I assume is the case now), has
benefits of its own. But at this point, we're reinventing databases.

@section[#:tag "c2e75"]{Exercise 2.75}

We can write @tt{make-from-mag-ang} in the new message-passing style
like this:

@codeblock{
(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos a)))
          ((eq? op 'imag-part) (* r (sin a)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else
           (error "Unknown op -- MAKE-FROM-MAG-ANG" op))))
  dispatch)
}

Just like the new @tt{make-from-real-imag}, this relies on using the magnitude
and angle arguments directly from the outer scope, rather than accessing them
through functions.

@section[#:tag "c2e76"]{Exercise 2.76}

We've learned three strategies for constructing generic systems with multiple
implementations.

The simplest thing we can do is to pool the implementations into a single
scope, differentiating between them by procedure name. This is how we ended
up with explicitly named @tt{real-part-rectangular} and @tt{real-part-polar}
procedures. To make this work, we have to pair a type tag with the data
to be able to use these operations, leaving us with @tt{rectangular?} and
@tt{polar?} procedures that are called inside the generic operations, such as
@tt{real-part}, in order to dispatch to the correct methods.

Alternatively, we could put these procedures into a table instead of directly
into the relevant scope, and access them via an operation and a type. For
example, we could get the equivalent of the @tt{real-part-rectangular} procedure
by wrapping around the essential operation of

@;examples[#:eval ev #:no-prompt
@codeblock{
 ((get 'real-part 'rectangular) z)
}

where the call to @tt{get} returns the procedure that is called on the complex
number object. Adding a new operation to our system is easy, because we can
create a new table and add implementations of the operation for our types
without having to modify any of the other tables. However, adding new types
forces us to modify all of the tables to expand our operations.

We can reverse this tradeoff by using "smarter" types that can dispatch
messages on their own. In this "message passing" style, adding new types is
easy, because they can be added independently. However, adding new operations
requires us to modify all of our existing types.

This is an instance of the
@hyperlink["http://c2.com/cgi/wiki?ExpressionProblem" "Expression Problem"].

@section[#:tag "c2e77"]{Exercise 2.77}

We're evaluating the expression @tt{(magnitude z)}, where @tt{z} is equal to
@tt{'(complex (rectangular (3 4)))}. Earlier, the procedure @tt{magnitude}
was defined like this:

@;examples[#:eval ev #:no-prompt
@codeblock{
 (define (magnitude z) (apply-generic 'magnitude z))
}

As is, this call wouldn't work because there is no entry in the table for
the @tt{magnitude} operation on the @tt{complex} type -- that is, for the
type wrapping the @tt{rectangular} and @tt{polar} types we created earlier.
However, all we need to do to fix this is to install the @tt{magnitude}
procedure itself as the entry for this type, as so:

@;examples[#:eval ev #:no-prompt
@codeblock{
 (put 'magnitude '(complex) magnitude)
}

With this in place, the call to @tt{magnitude} can be expanded like this
(I've simplified some of the steps for readability):

@racketblock[
 (magnitude '(complex (rectangular (3 4))))
 (apply-generic 'magnitude '(complex (rectangular (3 4))))
 (apply (get 'magnitude (type-tag '(complex (rectangular (3 4))))) (contents '(complex (rectangular (3 4)))))
 (apply (get 'magnitude 'complex) (contents '(complex (rectangular (3 4)))))
 (apply (get 'magnitude 'complex) '(rectangular (3 4)))
 (apply magnitude '(rectangular (3 4)))
 (apply-generic 'magnitude '(rectangular (3 4)))
 (apply (get 'magnitude (type-tag '(rectangular (3 4)))) (contents '(rectangular (3 4))))
 (apply (get 'magnitude 'rectangular) (3 4))
]

@tt{apply-generic} gets called twice, or once for each layer of typing our
generic @tt{complex} object has. In the first call, @tt{magnitude} dispatches
to itself, and in the second call, the procedure for the given type of the
object (in this case, @tt{rectangular}).

@section[#:tag "c2e78"]{Exercise 2.78}

We are asked to remove a layer of our typing for pure Scheme numbers, knowing
that the language is capable of discerning whether an object is a number. We
can do this by adding calls to @tt{number?} to @tt{attach-tag}, @tt{type-tag},
and @tt{contents}, probably like this:

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (type-tag datum)
   (if (pair? datum)
       (car datum)
       (if (number? datum)
           'scheme-number
           (error "Bad tagged datum -- TYPE-TAG" datum))))

 (define (contents datum)
   (if (pair? datum)
       (cdr datum)
       (if (number? datum)
           datum
           (error "Bad tagged datum -- CONTENTS" datum))))

 (define (attach-tag type-tag contents)
   (if (and
        (eq? type-tag 'scheme-number)
        (number? contents))
       contents
       (cons type-tag contents)))
 ]

I've elected to check the type of @tt{contents} and the given @tt{type-tag}
in @tt{attach-tag} for extra safety.

@section[#:tag "c2e79"]{Exercise 2.79}

@bold{TODO: Explain and verify}

@;examples[#:eval ev #:no-prompt
@codeblock{
(define (install-equ?-predicate)
  (define (equ?-scheme-number x y) (= x y))
  (define (equ?-rational x y)
    (and (= (numer x) (numer y))
         (= (denom x) (denom y))))
  (define (equ?-complex z1 z2)
    (and (= (real-part z1) (real-part z2))
         (= (imag-part z1) (imag-part z2))))
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y)
         (attach-tag 'scheme-number (equ?-scheme-number x y))))
  (put 'equ? '(rational rational)
       (lambda (x y)
         (attach-tag 'rational (equ?-rational x y))))
  (put 'equ? '(complex complex)
       (lambda (z1 z2)
         (attach-tag 'complex (equ?-complex z1 z2)))))

(define (equ? x y) (apply-generic 'equ? x y))
}

@section[#:tag "c2e80"]{Exercise 2.80}

@bold{TODO}

@section[#:tag "c2e81"]{Exercise 2.81}

Louis Reasoner wants to install coercions from types to themselves, because
@tt{apply-generic} may try to do this is a procedure isn't found. Of course,
the only reason this would happen is if an operation on the two types couldn't
be found in the first place, and coercing a type to itself won't change that
(this is Loose Reasoner, after all), but let's play along anyway.

So suppose we have a new procedure for exponentiation only defined on Scheme
numbers, created like this:

@;examples[#:eval ev #:no-prompt
@codeblock{
 (define (exp x y) (apply-generic 'exp x y))

 (put 'exp '(scheme-number scheme-number)
      (lambda (x y) (tag (expt x y))))
}

If we try to call this procedure on two complex numbers, @tt{apply-generic}
won't be able to find a procedure using @tt{get}, and so will attempt to
coerce the complex numbers to complex numbers and see if there is a procedure
then. But looking at how @tt{apply-generic} is defined, we can see that we
don't have to write these self-coercions:

@;examples[#:eval ev #:no-prompt
@codeblock{
(let ((t1->t2 (get-coercion type1 type2))
      (t2->t1 (get-coercion type2 type1)))
  (cond (t1->t2
         (apply-generic op (t1->t2 a1) a2))
        (t2->t1
         (apply-generic op a1 (t2->t1 a2)))
        (else
         (error "no method for these types"
                (list op type-tags)))))
}

If we don't install self-coercions into the table, neither @tt{t1->t2} nor
@tt{t2->t1} will exist, and the call to @tt{exp} will yield and error as
expected. However, if we do install the self-coercions, @tt{apply-generic}
will be called again with one "coerced" argument.

But of course, the next time through @tt{apply-generic}, the same thing
will happen -- a suitable procedure won't be found, and so the first argument
will be "coerced" and @tt{apply-generic} will be called again. Installing these
procedures will make @tt{apply-generic} nonterminating whenever a procedure
for two arguments of the same type isn't found. This is clearly wrong.

We could modify @tt{apply-generic} to not attempt to look up coercions if
the types are the same. It might look something like this:

@codeblock{
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (and (= (length args) 2)
                   (not (eq? (car type-tags)
                             (cadr type-tags))))
              ;; ...
              (error "No method for these types"
                     (list op type-tags)))))))
}

You could also check if the types are equal inside the @tt{if} (conveniently
after you have names for them), but I prefer this a little bit because it
doesn't increase the number of exit points in the procedure (that is, I would
need to add another call to @tt{error}).

@section[#:tag "c2e82"]{Exercise 2.82}

@bold{TODO}

@section[#:tag "c2e83"]{Exercise 2.83}

@bold{This is half-baked}

@;examples[#:eval ev #:no-prompt
@codeblock{
(define (install-raise-package)
  (define (raise-integer-rational x)
    (attach-tag 'rational (make-rational x 1)))
  (define (raise-rational-real x)
    (attach-tag 'real (make-real (/ (numer x) (denom x)))))
  (define (raise-real-complex x)
    (attach-tag 'complex (make-complex-from-real-imag x 0)))
  (put 'raise 'integer raise-integer-rational)
  (put 'raise 'rational raise-rational-real)
  (put 'raise 'real raise-real-complex))

(define (raise x)
  ((get 'raise (type-tag x)) x))
}

@section[#:tag "c2e84"]{Exercise 2.84}

@bold{TODO}

@section[#:tag "c2e85"]{Exercise 2.85}

@bold{TODO}

@section[#:tag "c2e86"]{Exercise 2.86}

@bold{TODO}

@section[#:tag "c2e87"]{Exercise 2.87}

@bold{TODO}

@section[#:tag "c2e88"]{Exercise 2.88}

@bold{TODO}

@section[#:tag "c2e89"]{Exercise 2.89}

@bold{TODO}

@section[#:tag "c2e90"]{Exercise 2.90}

@bold{TODO}

@section[#:tag "c2e91"]{Exercise 2.91}

@bold{TODO}

@section[#:tag "c2e92"]{Exercise 2.92}

@bold{TODO}

@section[#:tag "c2e93"]{Exercise 2.93}

@bold{TODO}

@section[#:tag "c2e94"]{Exercise 2.94}

@bold{TODO}

@section[#:tag "c2e95"]{Exercise 2.95}

@bold{TODO}

@section[#:tag "c2e96"]{Exercise 2.96}

@bold{TODO}

@section[#:tag "c2e97"]{Exercise 2.97}

@bold{TODO}
