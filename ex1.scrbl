#lang scribble/manual

@require[scribble/lp]

@title[#:version "" #:style 'toc]{Chapter 1}

@local-table-of-contents[]

@section{Exercise 1.1}

Omitted.
@section{Exercise 1.2}

@chunk[<ex1-2>
(define (ex1-2)
  (/ (+ 5 (+ 4 (- 2 (- 3 (+ 6 (/ 4 5))))))
     (* 3 (* (- 6 2) (- 2 7)))))
]

@section{Exercise 1.3}

@chunk[<ex1-3>
(define (ex1-3 a b c)
  (define (sqr x) (* x x))
  (define (sum-sq lst)
    (+ (sqr (car lst)) (sqr (cadr lst))))
  (define (two-largest a b c)
    (remove (min a b c) (list a b c)))
  (sum-sq (two-largest a b c)))
]

@section{Exercise 1.4}

This procedure adds the absolute value of @tt{b} to @tt{a}. To do this, it
selects the operation to apply on @tt{a} with @tt{b} based on whether @tt{b} is
greater than @tt{0}. If @tt{b > 0}, then @tt{b = abs(b)} and can be added to
@var{a}. Otherwise, @tt{-b = abs(b)} and so @tt{a - b = a + abs(b)}. Note that
@tt{-b = abs(b)} for the case where @tt{b = 0}.

@chunk[<a-plus-abs-b>
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
]

@section{Exercise 1.5}

In an applicative-order interpreter, the example will never
evaluate because the operand (@tt{p}) cannot be evaluated. In
a normal-order interpreter, (@tt{p}) would not need to be evaluated
until the alternate condition of the if condition is reached --
and since it won't be reached because the @tt{x} argument is @tt{0}, the
example will evaluate to @tt{0}.

@section{Exercise 1.6}

The @tt{sqrt-iter} procedure using the @tt{new-if} procedure never terminates.
This has to do with how all operands are evaluated, unlike with the
special form @tt{if}.

@bold{TODO: Elaborate}

@section{Exercise 1.7}

@tt{good-enough?} is inadequate for computing the square roots of small
numbers because it allows for too large a margin of error. The procedure
terminates if @tt{(square guess)} is within @tt{0.001} of @tt{x}. If @tt{0.001}
is large enough relative to @tt{x} -- or even larger than @tt{x} -- then
@tt{guess} will not necessarily be very close to the real answer.

As an example, @tt{(sqrt 0.0001)} evaluates to @tt{0.032}. The
square of this is @tt{0.001024}, which is within @tt{0.001} of @tt{0.0001}, and
is therefore an acceptable answer according to @tt{good-enough?}, while it is
actually not an acceptable answer at all.

@tt{good-enough?} has problems with large square roots because the constant
acceptable difference between the square of the guess of the square root and
the actual number is too small to be realistically reached?

A better @tt{sqrt} procedure using a @tt{good-enough?} test based on observing
the relative change between guesses and accepting a guess when this is small:

@chunk[<sqrt>
(define (sqrt x)
    (sqrt-iter x 1.0 0.0))
]

(The choice of starting guesses of 1 and 0 is arbitary.)

@chunk[<sqrt-iter>
(define (sqrt-iter x guess previous-guess)
    (if (good-enough? guess previous-guess) guess
        (sqrt-iter x (improve guess x) guess)))
]

@chunk[<good-enough?>
(define (good-enough? guess previous-guess)
    (< (abs (- guess previous-guess)) good-enough-factor))
]

I leave @tt{good-enough-factor} to be defined somewhere else, to vary for testing.

Using these procedures and a @tt{good-enough-factor} of @tt{0.001}, I can compute
@tt{(sqrt 0.0001} to be approximately @tt{0.010000714}, which is very close to the
correct answer. And with a @tt{good-enough-factor} of @tt{0.0000001}, it evaluates
to exactly @tt{0.01}.

@bold{TODO: Test very large numbers}

@section{Exercise 1.8}

To make this exercise more interesting, I've written an iteration procedure which
takes an @tt{improve} function as an argument. This means that the only difference
between a procedure computing square roots and a procedure computing cube roots is
the choice of improvement function. This generalization is handled differently
later in the chapter.

(I use my previously-defined @tt{good-enough?} procedure with a @tt{good-enough-factor}
of @tt{0.001}.)

@chunk[<cube-root>
(define (cube-root x)
    (root-iter x 1.0 0.0 cube-root-improve))
]

@chunk[<root-iter>
(define (root-iter x guess previous-guess improve)
    (if (good-enough? guess previous-guess) guess
        (root-iter x (improve guess x) guess improve)))
]

@chunk[<cube-root-improve>
(define (cube-root-improve x guess)
    (/
     (+
      (/ x (* guess guess))
      (* 2 guess))
     3))
]

@section{Exercise 1.9}

The evaluation of the expression @tt{(+ 4 5)} with the first procedure using the substition
model is as follows:

@verbatim{
(+ 4 5)
(inc (+ (dec 4) 5))
(inc (+ 3 5))
(inc (inc (+ (dec 3) 5)))
(inc (inc (+ 2 5)))
(inc (inc (inc (+ (dec 2) 5))))
(inc (inc (inc (+ 1 5))))
(inc (inc (inc (inc (+ (dec 1) 5)))))
(inc (inc (inc (inc (+ 0 5)))))
(inc (inc (inc (inc 5))))
(inc (inc (inc 5)))
(inc (inc 7))
(inc 8)
9
}

The first procedure generates a recursive process.

The evaluation using the second procedure generates an iterative process:

@verbatim{
(+ 4 5)
(+ (dec 4) (inc 5))
(+ 3 6)
(+ (dec 3) (inc 6))
(+ 2 7)
(+ (dec 2) (inc 7))
(+ 1 8)
(+ (dec 1) (inc 8))
(+ 0 9)
9
}

@section{Exercise 1.10}

The following evaluations using the substitution model are tedious, and are sometimes
simplified based on the results of earlier derivations, because the number of steps
required to calculate this procedure grows very quickly.

First, @tt{(A 1 10)}. First, expanding the arguments to @tt{A} until finding a base case:

@verbatim{
(A 1 10)
(A (-1 1) (A 1 (- 10 1)))
(A 0 (A 1 9))
(A 0 (A (- 1 1) (A 1 (- 9 1))))
(A 0 (A 0 (A 1 8)))
(A 0 (A 0 (A 0 (A 1 7))))
(A 0 (A 0 (A 0 (A 0 (A 1 6)))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 1 5))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 4)))))))
...
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 1))))))))))
}

Now, we know that @tt{(A 1 1)} evaluates to @tt{2}. Building on this,
@tt{(A 0 (A 1 1)} is the same as @tt{(A 0 2)}, which evaluates to @tt{(* 2 y)},
or @tt{4} in this case. We could use this to substitute in values of calls of @tt{A}
until finding an answer, but the answer is simpler to find directly. Notice the following:

@verbatim{
(A 0 1) = 2 * 1 = 2
(A 0 (A 0 1)) = 2 * (A 0 1) = 4
(A 0 (A 0 (A 0 1))) = 2 * (2 * (A 0 1)) = 8
(A 0 n) = 2^n
}

The answer is @tt{2^10 = 1024}

Next, @tt{(A 2 4)}. To solve this in fewer explicit steps, we use the result above
and completely skip simple math procedures.

@verbatim{
(A 2 4)
(A 1 (A 2 3))
(A 1 (A 1 (A 2 2)))
(A 1 (A 1 (A 1 (A 2 1))))
}

@tt{(A 2 1) = 2}, so:

@verbatim{
(A 1 (A 1 (A 1 2)))
(A 1 (A 1 (A 0 (A 1 1))))
(A 1 (A 1 (A 0 2)))
(A 1 (A 1 4))
}

This is a substitution we already know how to make. Accelerating the process:

@verbatim{
(A 1 16)
2^16
}

Finally, @tt{(A 3 3)}

@verbatim{
(A 3 3)
(A 2 (A 3 2))
(A 2 (A 2 (A 3 1)))
(A 2 (A 2 2))
(A 2 (A 1 (A 2 1)))
(A 2 (A 1 2))
(A 2 4)
2^16
}

In short, Ackermann's function is @emph{very} recursive. For the rest of the exercise, we consider
procedures that evaluate @tt{A} for fixed @tt{x} values.

@chunk[<f-ack>
(define (f n) (A 0 n))
]

@tt{f} evaluates a base case, with a result of @tt{2 * n}.

@chunk[<g-ack>
(define (g n) (A 1 n))
]

@tt{g} generalizes the first expansion we did, which evaluates to @tt{2 ^ n}.

@chunk[<h-ack>
(define (h n) (A 2 n))
]

@tt{h} generalizes the second expansion.  Notice a few cases:

@verbatim{
(h 1) = (A 2 1) = 2
(h 2) = (A 2 2) = (A 1 (A 2 1)) = (A 1 2) = 2^2 = 4
(h 3) = (A 2 3) = 2^(2^4) = 16
(h 4) = (A 2 4) = 2^(2^(2^4))) = 2^16}

Put concisely, @tt{(h n) = 2^(h (dec n))}.

@section{Exercise 1.11}

@chunk[<f-rec>
(define (f-rec n)
  (if (< n 3) n
      (+ (f-rec (- n 1))
         (* 2 (f-rec (- n 2)))
         (* 3 (f-rec (- n 3))))))
]

@chunk[<f-iter>
(define (f-iter n)
  (define (loop i a b c)
    (let ((f_i (+ a (* 2 b) (* 3 c))))
      (if (= n i) f_i
          (loop (+ i 1) f_i a b))))
  (if (< n 3) n
      (loop 3 2 1 0)))
]

@section{Exercise 1.12}

This procedure calculates the value in Pascal's triangle at
depth @tt{depth} and column @tt{column}. Both the depth and
column are 0-indexed.

@chunk[<pascal-value>
(define (pascal-value depth col)
  (cond ((= 0 col) 1)
        ((= depth col) 1)
        (else (+ (pascal-value (- depth 1) (- col 1))
                 (pascal-value (- depth 1) col)))))
]

We can define two more procedures to (primitively) print the
triangle:

@chunk[<make-new-row>
(define (make-new-row depth)
  (define (loop depth col str)
    (if (> col depth) str
        (string-append str 
                       (number->string (pascal-value depth col))
                       (loop depth (+ col 1) " "))))
  (loop depth 0 ""))
]

@chunk[<pascals-triangle>
(define (pascals-triangle depth)
  (display (make-pascal-triangle 0 depth "")))
]

However, I'm not satisfied with this recursive solution. It is
clear to me that, instead of tree-recursively generating every
value in the tree individually, we can generate the next row
of the tree given the bottom row in the current one. We can do
this by storing the tree as a list of lists (where new rows are
added to the @bold{front} of the list), and making new rows of
the tree by making a list of all the pairs of values in the last
row and mapping @tt{+} over them.

Of course, I could've also calculated values using binomial coefficients
by row, but I'm more interested in treating this like a data structure
juggling problem. Constructing and extending lists of lists can
be tricky (especially since we don't have mutation yet), and it
happens that this is a good problem for exploring one way to do this.

It is crucial to the simplicity of implementation that the lists
of lists be constructed by prepending -- making a new list by
@tt{cons}ing the new inner list with the old list itself. I haven't
bothered to reverse the rows of the entire tree, so it appears
upside-down -- but luckily for us, every row is the same forwards
and backwards, so this construction has no effect on the tree
itself.

I much prefer having a data structure holding an instance of
Pascal's triangle collected and returned. It seems to me that writing
procedures consuming this structure can be more general than those
that need to directly generate values in the triangle themselves, as
seen above.

There is a trick in @tt{make-pairs}: I add a new pair @tt{'(0 1)} to the front
and back of the pairs list. This is done to automatically generate the
@tt{1}s on the outside of the triangle without special cases. Also, @tt{make-pairs}
will generate no pairs other than these padding ones for the row at depth @tt{0},
@tt{'(1)}, because @tt{(cdr '(1))} is @tt{null}.

Unlike the exercise directs, this procedure also generates an iterative
process.

@chunk[<empty?>
(define (empty? l)
  (eq? nil l))
]

@chunk[<pascal-triangle>
(define (pascal-triangle depth)
  (define (make-pairs l)
    (define (loop l pairs)
      (if (null? (cdr l)) (cons '(0 1) pairs)
          (loop (cdr l)
                (cons (list (car l) (cadr l)) pairs))))
    (loop l '((0 1))))
  (define (compute-next-row row)
    (if (empty? row) '(1)
        (map
         (lambda (p) (+ (car p) (cadr p)))
         (make-pairs row))))
  (define (triangle-loop current-depth current-triangle)
    (if (> current-depth depth) current-triangle
        (triangle-loop (+ current-depth 1)
                       (cons (compute-next-row (car current-triangle))
                             current-triangle))))
  (triangle-loop 1 '((1))))
]

@section{Exercise 1.13}

@bold{Statement:} @tt{Fib(n)} is the closest integer to @tt{(ø^n) / √5}, where
@tt{ø = (1 + √5) / 2}.

Hint: Let @tt{ψ = (1 - √5) / 2}. The closest integer is then @tt{(ø^n - ψ^n) / √5}.

@bold{Base case:} @tt{n = 0}.

@verbatim{
Fib(0) = (Ø^0 - ψ^0) / √5 = 0
}

@bold{Inductive case:} Assume the statement holds for all @tt{k} up to @tt{n}.
Prove that it holds for @tt{n + 1}.

Note that, by the definition of @tt{Fib}, @tt{Fib(k + 1) = Fib(k) + Fib(k - 1)}. So,

@verbatim{
(ø^(k+1) - ψ^(k+1)) / √5 = (ø^k - ψ^k) / √5 + (ø^(k-1) - ψ^(k-1)) / √5
}

Multiplying both sides by @tt{√5} and rearranging, we get

@verbatim{
ø^(k+1) - ψ^(k+1) = ø^k + ø^(k-1) - ψ^k - ψ^(k-1)
ø^(k-1) * ø^2 - ψ^(k-1) * ψ^2 = ø^(k-1) * (ø + 1) - ψ^(k-1) * (ψ + 1)
}

Using the known values of @tt{ø} and @tt{ψ}, it is easy to verify that
both @tt{ø^2 = ø + 1} and @tt{ψ^2 = ψ + 1}. However, we can show this another
way. Note that these are both expressions of the form @tt{x^2 = x + 1}.
Turning this into @tt{x^2 - x - 1 = 0}, we can use the quadratic formula:

@verbatim{
x = (1 +/- √(1 - 4(-1))) / 2
= (1 +/- √5) / 2
= ø or ψ
}

Therefore, the equality @tt{Fib(k + 1) = Fib(k) + Fib(k - 1)} holds, meaning that,
given our inductive assumption, the value of @tt{Fib(k + 1)} is correct.
This method of computing Fibonacci numbers works.

@bold{TODO: Look this over between two and ten more times, and be more explicit}

@section{Exercise 1.14}

(Tree not included.)

To figure out the order of growth of the number of steps used in this process
(from now on called the time complexity), it's easiest to start with the case
where @tt{kinds-of-coins} is @tt{1} and then work up to the starting value of
@tt{5}.

@tt{(cc n 1)} evaluates like this:

@verbatim{
(cc n 1)
(+ (cc n 0) (cc (- n (first-denomination 1)) 1))
(+ (cc n 0) (cc (- n 1) 1))
}

Since @tt{(cc n 0)} simply returns @tt{1}, it has time complexity @tt{O(1)}. And
since every call of @tt{cc} calls @tt{cc} one more time until eventually reaching
an @tt{O(1)} base case, @tt{(c n 1)} has time complexity @tt{O(n)}.

Now suppose we want to evaluate @tt{(cc n 2)}:

@verbatim{
(cc n 2)
(+ (cc n 1) (cc (- n 5) 2))
}

Since @tt{5} is subtracted each time from @tt{n}, there are @tt{n / 5} calls to
@tt{cc} with a @tt{kinds-of-coins} value of @tt{2}. And each of these calls
@tt{cc} with a @tt{kinds-of-coints} value of @tt{1}. Therefore, calls to @tt{(cc n 2)}
have time copmlexity @tt{(n / 5) * O(n) = O(n^2)}.

The process continues similarly: @tt{(cc n 3)} has time complexity @tt{(n / 10) * O(n^2) = O(n^3)},
@tt{(cc n 4)} has time complexity @tt{(n / 25) * O(n^3) = O(n^4)}, and @tt{(cc n 5)}, our standard
call to @tt{cc}, has time complexity @tt{(n / 50) * O(n^4) = O(n^5)}.

The important choices in finding the time complexity of the whole process were to start
with a base case with a simple time complexity and to notice that incrementing
@tt{kinds-of-coins} gave a process with a time complexity that could be expressed in terms
of the last one.

@bold{TODO: Space complexity}

@section{Exercise 1.15}

@bold{TODO}

@section{Exercise 1.16}

@chunk[<fast-expt-iter>
(define (fast-expt-iter b n)
  (define (loop a b i)
    (cond ((= i 0) a)
          ((even? i) (loop a (square b) (/ i 2)))
          (else (loop (* a b) b (- i 1)))))
  (loop 1 b n))
]

@bold{TODO: Explain}

@section{Exercise 1.17}

@chunk[<fast-times>
(define (fast-times a b)
  (cond ((= b 0) 0)
        ((even? b) (double (fast-times a (halve b))))
        (else (+ a (fast-times a (- b 1))))))
]

@bold{TODO: Explain}

@section{Exercise 1.18}

@chunk[<fast-times-iter>
(define (fast-times-iter a b)
  (define (loop x a b)
    (cond ((= b 0) x)
          ((even? b) (loop x (double a) (halve b)))
          (else (loop (+ a x) a (- b 1)))))
  (loop 0 a b))
]

@bold{TODO: Explain}

@section{Exercise 1.19}

@chunk[<fib>
(define (fib n)
  (fib-iter 1 0 0 1 n))
]

@chunk[<fib-iter>
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q))
                   (+ (square q) (* 2 p q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))
]

@bold{TODO: Explain}

@section{Exercise 1.20}

@bold{TODO: Show normal-order evaluation}

Under normal-order evaluation, @tt{remainder} is called 18 times.

@bold{TODO: Show applicative-order evaluation}

Under applicative-order evaluation, @tt{remainder} is called 4 times.

@section{Exercise 1.21}

The smallest divisor of @tt{199} is @tt{199}.

The smallest divisor of @tt{1999} is @tt{1999}.

The smallest divisor of @tt{19999} is @tt{7}.
                                     
@section{Exercise 1.22}

A lot of the exercises in this book are more interesting than they appear. I'm
going to pick on this one for a little bit.

We are asked to write a procedure @tt{search-for-primes} to find primes greater
than certain numbers, and to investigate whether the running times needed to determine
if these numbers are prime increases as the asymptotic complexity of the primality-testing
algorithm we're using suggests. This procedure is meant to check for primality "of consecutive
odd integers in a specified range". With this range specified by a minimum and maximum value,
the procedure could be written like this:

@chunk[<search-for-primes>
(define (search-for-primes start end)
  (define (loop current end)
    (if (> current end) (newline)
         (begin
           (timed-prime-test current)
           (loop (+ current 2) end))))
  (cond ((not (< start end)) (search-for-primes end start))
        ((divides? 2 start) (loop (+ start 1) end))
        (else (loop start end))))
]

Here, I chose to use an inclusive range, and chose to switch the range bounds if @tt{start} was
greater than @tt{end}, rather than throwing an error.

This procedure is fully capable of completing the exercise, as we shall see later, but I'm
not satisfied with it. Its use of @tt{timed-prime-test} means that the results from the function
are printed, rather than returned in a useful way. And trying to use this procedure in the way
we are intended, to find the first three primes greater than @tt{1000}, @tt{10000}, etc is awkward,
needing trial and error. This function also contains a subtle bug. I think we can do better.

First, a function for computing the first @tt{n} primes greater than a certain number:

@chunk[<next-primes>
(define (next-primes start num)
  (define (loop current-number current-list)
    (cond ((= (length current-list) num) current-list)
          ((prime? current-number)
           (loop (+ current-number 2) (append current-list (list current-number))))
          (else (loop (+ current-number 2) current-list))))
  (cond ((< start 2) (error "can't start with a number less than 2"))
        ((= start 2) (loop 3 '(2)))
        ((divides? 2 start) (loop (+ start 1) '()))
        (else (loop (+ start 2) '()))))
]

Like much of the code I write, this uses an inner recursive loop procedure that assumes a simpler
set of inputs to reduce the inner conditions, with the outer procedure managing the interface
to this procedure. Besides being expressed with tail recursion, I find the simplifying assumptions
make the main algorithm easier to reason about -- in this case, getting rid of the awkwardness where
all even numbers are not prime except @tt{2}. This handles a case not properly handled by
@tt{search-for-primes}.

Now, we could pass these primes off to @tt{timed-prime-test}. But this
procedure deserves examination.  It manages the calling of @tt{runtime} itself,
which works well enough if you want to test the run time of primality
testing. But what if you didn't? You would have to write a new procedure to
test the run time yourself.

Instead, we can write a procedure to test the run time of any arbitrary procedure passed to it:

@chunk[<time-proc>
(define (time-proc proc . args)
  (let ((start-time (runtime)))
    (apply proc args)
    (- (runtime) start-time)))
]

This procedure takes a procedure as the first argument and executes it with the rest of its
given arguments passed as arguments to the given procedure, calling @tt{runtime} before
and after and returning the difference, much like how @tt{timed-prime-test} works. This is
similar to procedures that exist in many dialects of Lisp, including Racket.

We can use this to get the runtimes we've been asked for like this:

@chunk[<1.22-map>
(map
  (lambda (x)
    (time-proc prime? x))
  (next-primes 1000 3))
]

We can generalize this to work for @tt{10000} and the rest, or we could not.

The exercise asks us to test execution times on testing for primality on the
the first three primes greater than @tt{1000}, @tt{10000}, @tt{100000}, and
@tt{1000000}. But this is not being careful enough. In my current environment
(DrRacket), both of these means of testing show much higher execution times the
first time any number is tested. And those first execution times seem to be
quite variable.  We have to decide whether the average first execution time is
what we want, or if we want the average after the runtime reaches stability.

There is also a cost to @tt{time-proc} -- these runtimes are consistently
higher than those from @tt{timed-prime-test}. However, this cost is somewhere
in the tens of microseconds. This is not even remotely large enough to outweigh
the benefits of generalization.

@bold{TODO: Actually answer the question.}

@section{Exercise 1.23}

@bold{TODO - I don't want to do more of this runtime stuff right now}

@section{Exercise 1.24}

@bold{TODO - I don't want to do more of this runtime stuff right now}

@section{Exercise 1.25}

She is correct that you can compute @tt{expmod} this way, but it requires
a bit of knowledge to be sure of. The long-form @tt{expmod} takes the @tt{remainder}
of every recursive call to @tt{expmod} modulo @tt{m}, while Alyssa's @tt{expmod}
only takes the remainder once, after the exponent has been calculated. Importantly,
it is true that taking the remainder once after exponentiation or multiple times
during its calculation does not change the result.

However, because Alyssa's procedure takes the remainder of a potentially much larger
number, it may be the case that this is slower than the original procedure, which
computes more, simpler remainders.

@bold{TODO: Show test results}

@section{Exercise 1.26}

By calling @tt{*} instead of @tt{square}, the arguments need to be calculated
twice instead of once. Since these arguments are calls to @tt{expmod}, this means
that the problem is reduced from one problem of size @tt{n} to two problems of
size @tt{n/2}. This makes the problem O(n) instead of O(logn).

@bold{TODO: Elaborate}

@section{Exercise 1.27}

Here is a procedure to test for congruence for a number @tt{n} with every integer @tt{1 <= a <= n - 1}:

@chunk[<congruence-test>
(define (congruence-test num)
  (define (loop current)
    (cond ((= current num) #t)
          ((not (= (expmod current num num) current)) #f)
          (else (loop (+ current 1)))))
  (loop 1))
]

It is trivial to verify that this procedure returns true for the given Carmichael numbers.

@section{Exercise 1.28}

@bold{TODO}

@section{Exercise 1.29}

This procedure computes integrals with Simpson's rule:

@chunk[<simpson-integral>
(define (simpson-integral f a b n)
  (define h (/ (- b a) n))
  (define (term k)
    (let ((y (f (+ a (* k h)))))
      (cond ((= k 0) y)
            ((= k n) y)
            ((divides? 2 k) (* 2 y))
            (else (* 4 y)))))
  (* (/ h 3) (sum term 1 inc n)))
]

The decision that made this simplest to write was to let @tt{a} and @tt{b} in the @tt{sum}
be @tt{1} and @tt{n-1}, respectively -- the indexes of the evaluated function values, rather
than the values themselves. This way, we can test for what to multiply the value by (@tt{1}, 
@tt{2}, or @tt{4}) by examining the index, rather than having to describe the rule using the sum
of multiple summations or something else more complicated than this.

Comparing outputs with the book:

@verbatim|{
> (integral cube 0 1 0.01)
0.24998750000000042
> (integral cube 0 1 0.001)
0.249999875000001
> (simpson-integral cube 0 1 100.0)
0.24999999999999992
> (simpson-integral cube 0 1 1000.0)
0.2500000000000003
}|

@section{Exercise 1.30}

@chunk[<sum>
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))
]

Curiously, in my environment, this procedure gives very slightly different answers compared to above:

@verbatim|{
> (integral cube 0 1 0.01)
0.24998750000000042
> (integral cube 0 1 0.001)
0.24999987500000073
> (simpson-integral cube 0 1 100.0)
0.25000000000000006
> (simpson-integral cube 0 1 1000.0)
0.25000000000000006
}|

@section{Exercise 1.31}

We can define a @tt{product} procedure very similarly to the first @tt{sum} procedure:

@chunk[<product>
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))
]

We can use this procedure to define factorial as follows:

@chunk[<factorial>
(define (factorial n)
  (product identity 1 inc n))
]

We can use this procedure to compute pi as follows:

@chunk[<pi-product>
(define (pi-product n)
  (define (numer n)
    (define (numer-term i)
      (if (divides? 2 i)
          (+ i 2)
          (+ i 1)))
    (product numer-term 1 inc n))
  (define (denom n)
    (define (denom-term i)
      (if (divides? 2 i)
          (+ i 1)
          (+ i 2)))
    (product denom-term 1 inc n))
  (* 4 (/ (numer n) (denom n))))
]

A @tt{product} procedure computing an iterative process is as follows:

@chunk[<product>
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (* result (term a)))))
  (iter a 1))
]

@section{Exercise 1.32}

A general @tt{accumulate} procedure can be written as follows:

@chunk[<accumulate-recur>
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))
]

We can write @tt{sum} and @tt{product} using @tt{accumulate}:

@chunk[<sum>
(define (sum term a next b)
  (accumulate + 0 term a next b))
]

@chunk[<product>
(define (product term a next b)
  (accumulate * 1 term a next b))
]

We can write an iterative @tt{accumulate} like this:

@chunk[<accumulate-iter>
(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b) result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))
]

@section{Exercise 1.33}

@chunk[<filtered-accumulate>
(define (filtered-accumulate predicate combiner null-value term a next b)
  (define (iter a result)
    (cond ((> a b) result)
          ((predicate a)
           (iter (next a) (combiner result (term a))))
          (else
           (iter (next a) result))))
  (iter a null-value))
]

@chunk[<sum-of-primes>
(define (sum-of-primes a b)
  (filtered-accumulate prime? + 0 identity a inc b))
]

@chunk[<product-of-relatively-prime>
(define (product-of-relatively-prime n)
  (define (is-relatively-prime? i)
    (= 1 (gcd i n)))
  (filtered-accumulate is-relatively-prime? * 1 identity 1 inc (- n 1)))
]

@section{Exercise 1.34}

Suppose we have the following procedure @tt{f}:

@chunk[<f-1-34>
(define (f g)
  (g 2))
]

If we ask the interpreter to evaluate @tt{(f f)}, the following happens:

@verbatim{
  (f f)
= (f 2)
= (2 2)
}

This fails, because @tt{2} is not a procedure.

@section{Exercise 1.35}

We can see that the golden ratio is the fixed point of the function @tt{f(x) = x + 1/x} simply
by evaluating:

@verbatim{
> (define (golden-transform x) (+ 1 (/ 1 x)))
> (define golden-start 1.61803398875)
> (golden-transform golden-start)
1.6180339887498547
> (golden-transform (golden-transform golden-start))
1.6180339887499102
> (golden-transform (golden-transform (golden-transform golden-start)))
1.6180339887498891
> (golden-transform (golden-transform (golden-transform (golden-transform golden-start))))
1.6180339887498971
}

This can also be derived simply from the definition of the golden ratio, which relates @tt{a}
to @tt{b} by @tt{(a + b) / a = a / b = phi}. The left side can be expanded to @tt{a / a + b / a}, or
@tt{1 + 1 / (a / b) = 1 + 1 / phi}.

We can then write a procedure to compute the golden ratio using @tt{fixed-point} as such:

@chunk[<golden-ratio>
(define golden-ratio
  (fixed-point golden-transform 1))
]

@section{Exercise 1.36}

I'm ommitting the change to @tt{fixed-point} to make it print successive approximations because
it is trivial -- I added two lines to display the guess and then print a newline at the beginning
of the @tt{try} procedure.

We can see the difference that average damping makes easily:

@chunk[<36-f>
(define (36-f x) (/ (log 1000) (log x)))
]

@verbatim{
> (fixed-point 36-f 2)
2
9.965784284662087
3.004472209841214
6.279195757507157
3.759850702401539
5.215843784925895
4.182207192401397
4.8277650983445906
4.387593384662677
4.671250085763899
4.481403616895052
4.6053657460929
4.5230849678718865
4.577114682047341
4.541382480151454
4.564903245230833
4.549372679303342
4.559606491913287
4.552853875788271
4.557305529748263
4.554369064436181
4.556305311532999
4.555028263573554
4.555870396702851
4.555315001192079
4.5556812635433275
4.555439715736846
4.555599009998291
4.555493957531389
4.555563237292884
4.555517548417651
4.555547679306398
4.555527808516254
4.555540912917957
4.555532270803653
}

@verbatim{
(fixed-point (lambda (y) (average y (36-f y))) 2)
2
5.9828921423310435
4.922168721308343
4.628224318195455
4.568346513136242
4.5577305909237005
4.555909809045131
4.555599411610624
4.5555465521473675
4.555537551999825
}

And this does calculate the fixed point of the function:

@verbatim{
> (expt 4.555537551999825 4.555537551999825)
1000.0046472054871
}

@section{Exercise 1.37}

A procedure for evaluating a continued fraction for up to depth @tt{k}:

@chunk[<cont-frac-recur>
(define (cont-frac n d k)
  (define (loop i)
    (if (= i k) (/ (n i) (d i))
      (/ (n i)
         (+ (d i) (loop (+ i 1))))))
  (loop 1))
]

Using an auxiliary procedure @tt{test-37}, we can see @tt{1/phi} being approximated:

@chunk[<test-37>
(define (test-37 k)
  (/ 1 (cont-frac (lambda (i) 1.0)
                  (lambda (i) 1.0)
                  k)))
]

@verbatim{
> (test-37 10)
1.6181818181818184
> (test-37 100)
1.618033988749895
}

Writing an iterative @tt{cont-frac} procedure is difficult until you
realize that it is much easier to create the fraction from the inside
out, starting from @tt{k} and moving to @tt{1}. Then the answer is
straightforward:

@chunk[<cont-frac-iter>
(define (cont-frac n d k)
  (define (loop current i)
    (if (= i 0) current
        (loop (/ (n i) (+ (d i) current))
              (- i 1))))
  (loop (/ (n k) (d k)) (- k 1)))
]

@section{Exercise 1.38}

Using an internally-defined @tt{denom} function, which reduces the index
by @tt{2} if it's greater than @tt{2} to make the logic simpler, we can
use @tt{cont-frac} to approximate @tt{e} using Euler's expansion like this:

@chunk[<euler-e-frac>
(define (euler-e-frac k)
  (define (denom i)
    (if (< i 3) i
        (let ((red-i (- i 2)))
          (if (= (remainder red-i 3) 0)
              (+ 2 (* 2 (/ red-i 3)))
              1))))
  (+ 2 (cont-frac (lambda (i) 1.0)
                  denom
                  k)))
]

Testing this procedure, we can see it does calculate @tt{e}:

@verbatim{
> (euler-e-frac 10)
2.7182817182817183
> (euler-e-frac 100)
2.7182818284590455
}

@section{Exercise 1.39}

@chunk[<tan-cf>
(define (tan-cf x k)
  (define (numer i)
    (if (= i 1) x (* (square x) -1.0)))
  (define (denom i)
    (- (* 2.0 i) 1.0))
  (cont-frac numer denom k))
]

@section{Exercise 1.40}

We want @tt{cubic} to be a procedure of three parameter @tt{a}, @tt{b}, and @tt{c}
that returns a procedure of one parameter @tt{x} that evaluates the function
@tt{x^3 + ax^2 + bx + c}. This can be done as so:

@chunk[<cubic>
(define (cubic a b c)
  (lambda (x)
    (+ (cube x)
       (* a (square x))
       (* b x)
       c)))
]

We can verify this works with an example, finding the root of the function
@tt{x^3 + 2x^2 + 3x - 5} (which is approximately 0.89456):

@verbatim{
> (newtons-method (cubic 2 3 -5) 1.0)
0.8945582482428005
}

@section{Exercise 1.41}

The procedure definition of @tt{double} is simple:

@chunk[<double>
(define (double f)
  (lambda (x) (f (f x))))
]

Evaluating @tt{(((double (double double)) inc) 5)} via the substitution method is
cumbersome, but we can build up the solution in parts. We know that
@tt{(double inc)} returns a procedure that increments its argument twice.
Applying @tt{double} to @tt{double} and applying this to @tt{inc} is going
to do the following:

@verbatim{
((double double) inc)
= ((lambda (x) (double (double x))) inc)
= (double (double inc))
}

The total number of increments is squared, creating a procedure that increments
four times. Similarly, @tt{((double (double double)) inc)} creates a procedure that
increments @tt{4^2 = 16} times. The answer to the expression is @tt{21}.

@bold{TODO: That cumbersome derivation?}

@section{Exercise 1.42}

Function composition is a powerful idea, but Scheme is expressive enough for
a self-explanatory implementation:

@chunk[<compose>
(define (compose f g)
  (lambda (x) (f (g x))))
]

@section{Exercise 1.43}

We can define @tt{repeated} simply using @tt{compose} from the last exercise:

@chunk[<repeated>
(define (repeated f times)
  (if (= times 1) f
      (compose f (repeated f (- times 1)))))
]

The main idea is that repeating @tt{f} @tt{t} times is equivalent to applying @tt{f}
to @tt{f} repeated @tt{t - 1} times, with a base case of @tt{f} repeated @tt{1} time,
which is just @tt{f}.

@section{Exercise 1.44}

@bold{TODO: Words}

@chunk[<smooth>
(define (smooth f)
  (lambda (x)
    (/
     (+ (f (- x dx))
        (f x)
        (f (+ x dx)))
     3)))
]

@chunk[<smooth>
(define (n-smooth f times)
  (repeated smooth times))
]

@section{Exercise 1.45}

The number of @tt{average-damp} calls needed for the @tt{n}th root to converge is
the floor of log2(n).

@chunk[<nth-root>
(define (nth-root x n)
  (fixed-point-of-transform (lambda (y) (/ x (expt y (- n 1))))
                            (repeated average-damp (floor (log-base n 2)))
                            1.0))
]

@bold{TODO: More words}

@section{Exercise 1.46}

@chunk[<iterative-improve>
(define (iterative-improve good-enough? improve-guess)
  (define (iter guess)
    (if (good-enough? guess) guess
        (iter (improve-guess guess))))
  iter)
]

The square root procedure is easy to rewrite using @tt{iterative-improve}.

@chunk[<sqrt-iterimp>
(define (sqrt x)
  ((iterative-improve
     (lambda (guess) (< (abs (- (square guess) x)) 0.001))
     (lambda (guess) (average guess (/ x guess))))
   1.0))
]

@tt{fixed-point} is also easy to rewrite, if you don't need to get the exact
same result. The procedure relies on comparing the current guess with the next
guess, and returning the next guess if they are close enough. However,
@tt{iterative-improve} returns the current guess if @tt{good-enough?}
holds. The two results will be very close (as they have to be, if
@tt{close-enough?} is going to return true), but not identical.

@chunk[<fixed-point-iterimp>
(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  ((iterative-improve
    (lambda (guess)
      (close-enough? guess (f guess)))
    f)
   first-guess))
]

At the moment, I don't see a way around this.
