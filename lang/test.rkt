;; File test.rkt from http://www.neilvandyke.org/racket-sicp/

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 1.1.1  Expressions

(check-expect (+ 137 349) 486)
(check-expect (- 1000 334) 666)
(check-expect (* 5 99) 495)
(check-expect (/ 10 5) 2)
(check-expect (+ 2.7 10) 12.7)

(check-expect (+ 21 35 12 7) 75)
(check-expect (* 25 4 12) 1200)

(check-expect (+ (* 3 5) (- 10 6)) 19)
(check-expect (+ (* 3 (+ (* 2 4) (+ 3 5))) (+ (- 10 7) 6)) 57)

;; 1.1.2  Naming and the Environment

(define size 2)

(check-expect size 2)
(check-expect (* 5 size) 10)

(define pi 3.14159)
(define radius 10)
(check-expect (* pi (* radius radius)) 314.159)
(define circumference (* 2 pi radius))
(check-expect circumference 62.8318)

;; 1.1.4  Compound Procedures

(define (square x) (* x x))

(check-expect (square 21) 441)

(check-expect (square (+ 2 5)) 49)

(check-expect (square (square 3)) 81)

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(check-expect (sum-of-squares 3 4) 25)

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))

(check-expect (f 5) 136)

;; 1.1.6  Conditional Expressions and Predicates

(define (abs:1 x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

(define (abs:2 x)
  (cond ((< x 0) (- x))
        (else x)))

(define (abs:3 x)
  (if (< x 0)
      (- x)
      x))

(define (>=:1 x y)
  (or (> x y) (= x y)))

(define (>=:2 x y)
  (not (< x y)))

;; 1.1.7  Example: Square Roots by Newton's Method

(define (sqrt:1 x)
  (the y (and (>= y 0)
              (= (square y) x))))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt:2 x)
  (sqrt-iter 1.0 x))

(check-expect (sqrt:2 9) 3.00009155413138)
(check-expect (sqrt:2 (+ 100 37)) 11.704699917758145)
(check-expect (sqrt:2 (+ (sqrt:2 2) (sqrt:2 3))) 1.7739279023207892)
(check-expect (square (sqrt:2 1000)) 1000.000369924366)

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(check-expect (new-if (= 2 3) 0 5) 5)
(check-expect (new-if (= 1 1) 0 5) 0)

;; 1.1.8  Procedures as Black-Box Abstractions

(define (square:2 x)
  (exp (double (log x))))

(define (double x) (+ x x))

(define (sqrt:3 x)
  (define (good-enough?:2 guess x)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve:2 guess x)
    (average guess (/ x guess)))
  (define (sqrt-iter:2 guess x)
    (if (good-enough?:2 guess x)
        guess
        (sqrt-iter:2 (improve guess x) x)))
  (sqrt-iter:2 1.0 x))

(define (sqrt:4 x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

;; 1.2.1  Linear Recursion and Iteration

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(define (factorial:2 n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))

(check-expect (factorial:2 6) 720)

(define (+:1 a b)
  (if (= a 0)
      b
      (inc (+:1 (dec a) b))))

(define (+:2 a b)
  (if (= a 0)
      b
      (+:2 (dec a) (inc b))))

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(A 1 10)

(A 2 4)

(A 3 3)

;; 1.2.2  Tree Recursion

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define (fib:2 n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))

(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

(define (cube x) (* x x x))

(define (p x) (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

;; 1.2.4  Exponentiation

(define (expt:2 b n)
  (if (= n 0)
      1
      (* b (expt:2 b (- n 1)))))

(define (expt:3 b n)
  (expt-iter b n 1))

(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b
                 (- counter 1)
                 (* b product))))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (even?:2 n)
  (= (remainder n 2) 0))

(define (*:2 a b)
  (if (= b 0)
      0
      (+ a (*:2 a (- b 1)))))

;; 1.2.5  Greatest Common Divisors

(define (gcd:2 a b)
  (if (= b 0)
      a
      (gcd:2 b (remainder a b))))

;; 1.2.6  Example: Testing for Primality

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(check-expect (prime? 1) #t)
(check-expect (prime? 2) #t)
(check-expect (prime? 3) #t)
(check-expect (prime? 4) #f)

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

;; TODO: Test that this generates an error?
;;
;; (check-expect (fast-prime? 1 100) #t)

(check-expect (fast-prime? 2 100) #t)
(check-expect (fast-prime? 3 100) #t)
(check-expect (fast-prime? 4 100) #f)

;; Exercise 1.22

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

;; Exercise 1.25

(define (expmod:2 base exp m)
  (remainder (fast-expt base exp) m))

;; Exercise 1.26

(define (expmod:3 base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (* (expmod:3 base (/ exp 2) m)
                       (expmod:3 base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod:3 base (- exp 1) m))
                    m))))

;; 1.3  Formulating Abstractions with Higher-Order Procedures

(define (cube:1.3 x) (* x x x))

;; 1.3.1  Procedures as Arguments

(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube:1.3 a) (sum-cubes (+ a 1) b))))

(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc:1.3.1 n) (+ n 1))

(define (sum-cubes:2 a b)
  (sum cube:1.3 a inc:1.3.1 b))

(check-expect (sum-cubes:2 1 10) 3025)

(define (identity:1.3.1 x) x)

(define (sum-integers:1.3.1:2 a b)
  (sum identity:1.3.1 a inc:1.3.1 b))

(check-expect (sum-integers:1.3.1:2 1 10) 55)

(define (pi-sum:1.3.1:2 a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(check-expect (* 8 (pi-sum:1.3.1:2 1 1000)) 3.139592655589783)

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(check-expect-approx (integral cube 0 1 0.01) 0.24998750000000042)
(check-expect-approx (integral cube 0 1 0.001) 0.249999875000001)

;; 1.3.2  Constructing Procedures Using Lambda

(define (pi-sum:1.3.2 a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b))

(define (integral:1.3.2 f a b dx)
  (* (sum f
          (+ a (/ dx 2.0))
          (lambda (x) (+ x dx))
          b)
     dx))

(define (plus4:1 x) (+ x 4))

(define plus4:2 (lambda (x) (+ x 4)))

(check-expect ((lambda (x y z) (+ x y (square z))) 1 2 3) 12)

(define (f:1.3.2:1 x y)
  (define (f-helper a b)
    (+ (* x (square a))
       (* y b)
       (* a b)))
  (f-helper (+ 1 (* x y))
            (- 1 y)))

(define (f:1.3.2:2 x y)
  ((lambda (a b)
     (+ (* x (square a))
        (* y b)
        (* a b)))
   (+ 1 (* x y))
   (- 1 y)))

(define (f:1.3.2:3 x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

(check-expect (let ((x 5))
                (+ (let ((x 3))
                     (+ x (* x 10)))
                   x))
              38)

(check-expect (let ((x 2))
                (let ((x 3)
                      (y (+ x 2)))
                  (* x y)))
              12)

(define (f:1.3.2:4 x y)
  (define a (+ 1 (* x y)))
  (define b (- 1 y))
  (+ (* x (square a))
     (* y b)
     (* a b)))

;; Exercise 1.34

(define (f:e.1.34 g)
  (g 2))

(check-expect (f:e.1.34 square) 4)

(check-expect (f:e.1.34 (lambda (z) (* z (+ z 1)))) 6)

;; 1.3.3  Procedures as General Methods

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))

(check-expect-approx (half-interval-method sin 2.0 4.0) 3.14111328125)

(check-expect-approx (half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3))
                                           1.0
                                           2.0)
                     1.89306640625)

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(check-expect-approx (fixed-point cos 1.0) 0.7390822985224023)

(check-expect-approx (fixed-point (lambda (y) (+ (sin y) (cos y)))
                                  1.0)
                     1.2587315962971173)

(define (sqrt:1.3.3 x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))

;; 1.3.4  Procedures as Returned Values

(define (average-damp f)
  (lambda (x) (average x (f x))))

(check-expect ((average-damp square) 10) 55)

(define (sqrt:1.3.4:1 x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define dx 0.00001)

(define (cube:1.3.4 x) (* x x x))

(check-expect-approx ((deriv cube:1.3.4) 5) 75.00014999664018)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt:1.3.4:2 x)
  (newtons-method (lambda (y) (- (square y) x))
                  1.0))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt:1.3.4:3 x)
  (fixed-point-of-transform (lambda (y) (/ x y))
                            average-damp
                            1.0))

(define (sqrt:1.3.4:4 x)
  (fixed-point-of-transform (lambda (y) (- (square y) x))
                            newton-transform
                            1.0))

;;EOF

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; $Id: test.rkt,v 1.4 2011/05/02 09:25:47 neilpair Exp $

;; Chapter 2

(define (linear-combination a b x y)
  (+ (* a x) (* b y)))

;; 2.1.1  Example: Arithmetic Operations for Rational Numbers

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define x:2.1.1:1 (cons 1 2))

(check-expect (car x:2.1.1:1) 1)
(check-expect (cdr x:2.1.1:1) 2)

(define x:2.1.1:2 (cons 1 2))

(define y:2.1.1:2 (cons 3 4))

(define z:2.1.1:2 (cons x:2.1.1:2 y:2.1.1:2))

(check-expect (car (car z:2.1.1:2)) 1)
(check-expect (car (cdr z:2.1.1:2)) 3)

(define (make-rat n d) (cons n d))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define one-half (make-rat 1 2))

;; TODO: check-expect-output
;;
;; (print-rat one-half)
;; 1/2

(define one-third (make-rat 1 3))

;; (print-rat (add-rat one-half one-third))
;; 5/6

;; (print-rat (mul-rat one-half one-third))
;; 1/6

;; (print-rat (add-rat one-third one-third))
;; 6/9

;; (define (make-rat n d)
;;   (let ((g (gcd n d)))
;;     (cons (/ n g) (/ d g))))
;;
;; (print-rat (add-rat one-third one-third))
;; 2/3

;; 2.1.2  Abstraction Barriers

(define (make-rat n d)
  (cons n d))

(define (numer x)
  (let ((g (gcd (car x) (cdr x))))
    (/ (car x) g)))

(define (denom x)
  (let ((g (gcd (car x) (cdr x))))
    (/ (cdr x) g)))

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;; 2.1.4  Extended Exercise: Interval Arithmetic

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

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

(define (make-interval a b) (cons a b))

(define (lower-bound x) 0) ;; Placeholder
(define (upper-bound x) 0) ;; Placeholder

;; Exercise 2.11

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

;; Exercise 2.13

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

;; 2.2.1  Representing Sequences

(define one-through-four (list 1 2 3 4))

(check-expect one-through-four '(1 2 3 4))
(check-expect (car one-through-four) 1)
(check-expect (cdr one-through-four) '(2 3 4))
(check-expect (car (cdr one-through-four)) 2)
(check-expect (cons 10 one-through-four) '(10 1 2 3 4))
(check-expect (cons 5 one-through-four) '(5 1 2 3 4))

(define (list-ref:2.2.1 items n)
  (if (= n 0)
      (car items)
      (list-ref:2.2.1 (cdr items) (- n 1))))

(define squares (list 1 4 9 16 25))

(check-expect (list-ref:2.2.1 squares 3) 16)

(define (length:2.2.1:1 items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define odds (list 1 3 5 7))

(check-expect (length:2.2.1:1 odds) 4)

(define (length:2.2.1:2 items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))

(define (append:2.2.1 list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(check-expect (append:2.2.1 squares odds) '(1 4 9 16 25 1 3 5 7))
(check-expect (append:2.2.1 odds squares) '(1 3 5 7 1 4 9 16 25))

;; Exercise 2.19

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

;; (cc 100 us-coins)
;; 292

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(define (except-first-denomination coin-values) 0) ; Placeholder
(define (first-denomination coin-values) 0) ; Placeholder

;; Mapping over lists

(define (scale-list:2.2.1:1 items factor)
  (if (null? items)
      nil
      (cons (* (car items) factor)
            (scale-list:2.2.1:1 (cdr items) factor))))

(check-expect (scale-list:2.2.1:1 (list 1 2 3 4 5) 10) '(10 20 30 40 50))

(define (map:2.2.1 proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map:2.2.1 proc (cdr items)))))

(check-expect (map:2.2.1 abs (list -10 2.5 -11.6 17)) '(10 2.5 11.6 17))
(check-expect (map:2.2.1 (lambda (x) (* x x)) (list 1 2 3 4))
              '(1 4 9 16))

(define (scale-list:2.2.1:2 items factor)
  (map:2.2.1 (lambda (x) (* x factor))
             items))

;; 2.2.2  Hierarchical Structures

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(define x:2.2.2 (cons (list 1 2) (list 3 4)))

(check-expect (length x:2.2.2) 3)
(check-expect (count-leaves x:2.2.2) 4)
(check-expect (list x:2.2.2 x:2.2.2) '(((1 2) 3 4) ((1 2) 3 4)))
(check-expect (length (list x:2.2.2 x:2.2.2)) 2)
(check-expect (count-leaves (list x:2.2.2 x:2.2.2)) 8)

(define (scale-tree:2.2.2:1 tree factor)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree:2.2.2:1 (car tree) factor)
                    (scale-tree:2.2.2:1 (cdr tree) factor)))))
(check-expect (scale-tree:2.2.2:1 (list 1 (list 2 (list 3 4) 5) (list 6 7))
                                  10)
              '(10 (20 (30 40) 50) (60 70)))

(define (scale-tree:2.2.2:2 tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree:2.2.2:2 sub-tree factor)
             (* sub-tree factor)))
       tree))

(define (sum-odd-squares:2.2.2:1 tree)
  (cond ((null? tree) 0)
        ((not (pair? tree))
         (if (odd? tree) (square tree) 0))
        (else (+ (sum-odd-squares:2.2.2:1 (car tree))
                 (sum-odd-squares:2.2.2:1 (cdr tree))))))

(define (even-fibs:2.2.2:1 n)
  (define (next k)
    (if (> k n)
        nil
        (let ((f (fib k)))
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1))))))
  (next 0))

;; TODO: Added from test-ch-1.ss.  Perhaps combine all into one file?
(define (square x) (* x x))

(check-expect (map square (list 1 2 3 4 5)) '(1 4 9 16 25))

;; Sequence Operations

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(check-expect (filter odd? (list 1 2 3 4 5)) '(1 3 5))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(check-expect (accumulate + 0 (list 1 2 3 4 5)) 15)
(check-expect (accumulate * 1 (list 1 2 3 4 5)) 120)
(check-expect (accumulate cons nil (list 1 2 3 4 5)) '(1 2 3 4 5))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(check-expect (enumerate-interval 2 7) '(2 3 4 5 6 7))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(check-expect (enumerate-tree (list 1 (list 2 (list 3 4)) 5)) '(1 2 3 4 5))

;; TODO: Added from test-ch-1.ss.  Perhaps combine all into one file?
(define (fib:2 n)
  (fib-iter 1 0 n))
(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))

(define (sum-odd-squares tree)
  (accumulate +
              0
              (map square
                   (filter odd?
                           (enumerate-tree tree)))))

(define (even-fibs:2.2.3 n)
  (accumulate cons
              nil
              (filter even?
                      (map fib:2
                           (enumerate-interval 0 n)))))

(define (list-fib-squares n)
  (accumulate cons
              nil
              (map square
                   (map fib:2
                        (enumerate-interval 0 n)))))

(check-expect (list-fib-squares 10) '(0 1 1 4 9 25 64 169 441 1156 3025))

(define (product-of-squares-of-odd-elements sequence)
  (accumulate *
              1
              (map square
                   (filter odd? sequence))))

(check-expect (product-of-squares-of-odd-elements (list 1 2 3 4 5)) 225)

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

;; Nested Mappings

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (prime? x) #f) ;; Placeholder

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))

(define (permutations s)
  (if (null? s)                    ; empty set?
      (list nil)                   ; sequence containing empty set
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

;; (define (queens board-size)
;;   (define (queen-cols k)
;;     (if (= k 0)
;;         (list empty-board)
;;         (filter
;;          (lambda (positions) (safe? k positions))
;;          (flatmap
;;           (lambda (rest-of-queens)
;;             (map (lambda (new-row)
;;                    (adjoin-position new-row k rest-of-queens))
;;                  (enumerate-interval 1 board-size)))
;;           (queen-cols (- k 1))))))
;;   (queen-cols board-size))

;; TODO: !!! Continue starting from 2.2.4 (picture language)

;; 2.3.1  Quotation

(check-expect (list 'a 'b) '(a b))
(check-expect (car '(a b c)) 'a)
(check-expect (cdr '(a b c)) '(b c))

(define (memq:2.3.1 item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq:2.3.1 item (cdr x)))))

(check-expect (memq:2.3.1 'apple '(pear banana prune)) #f)
(check-expect (memq:2.3.1 'apple '(x (apple sauce) y apple pear)) '(apple pear))

;; Exercise 2.55

(check-expect (car ''abracadabra) 'quote)

;; 2.3.2  Example: Symbolic Differentiation

;; The differentiation program with abstract data

(define (deriv:1 exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum:1 (deriv:1 (addend exp) var)
                     (deriv:1 (augend exp) var)))
        ((product? exp)
         (make-sum:1
          (make-product:1 (multiplier exp)
                          (deriv:1 (multiplicand exp) var))
          (make-product:1 (deriv:1 (multiplier exp) var)
                          (multiplicand exp))))
        (else
         (error "unknown expression type -- DERIV" exp))))

;; Representing algebraic expressions

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum:1 a1 a2) (list '+ a1 a2))

(define (make-product:1 m1 m2) (list '* m1 m2))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))



(check-expect (deriv:1 '(+ x 3) 'x) '(+ 1 0))
(check-expect (deriv:1 '(* x y) 'x) '(+ (* x 0) (* 1 y)))
(check-expect (deriv:1 '(* (* x y) (+ x 3)) 'x) 
              '(+ (* (* x y) (+ 1 0))
                  (* (+ (* x 0) (* 1 y))
                     (+  x 3))))

;;

(define (make-sum:2 a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product:2 m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (deriv:2 exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum:2 (deriv:2 (addend exp) var)
                     (deriv:2 (augend exp) var)))
        ((product? exp)
         (make-sum:2
          (make-product:2 (multiplier exp)
                          (deriv:2 (multiplicand exp) var))
          (make-product:2 (deriv:2 (multiplier exp) var)
                          (multiplicand exp))))
        (else
         (error "unknown expression type -- DERIV" exp))))

(check-expect (deriv:2 '(+ x 3) 'x) 1)
(check-expect (deriv:2 '(* x y) 'x) 'y)
(check-expect (deriv:2 '(* (* x y) (+ x 3)) 'x) '(+ (* x y) (* y (+ x 3))))

;; 2.3.3  Example: Representing Sets

;; Sets as unordered lists

(define (element-of-set?:1 x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set?:1 x (cdr set)))))

(define (adjoin-set:1 x set)
  (if (element-of-set?:1 x set)
      set
      (cons x set)))

(define (intersection-set:1 set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set?:1 (car set1) set2)        
         (cons (car set1)
               (intersection-set:1 (cdr set1) set2)))
        (else (intersection-set:1 (cdr set1) set2))))

;; Sets as ordered lists

(define (element-of-set?:2 x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set?:2 x (cdr set)))))

(define (intersection-set:2 set1 set2)
  (if (or (null? set1) (null? set2))
      '()    
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set:2 (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set:2 (cdr set1) set2))
              ((< x2 x1)
               (intersection-set:2 set1 (cdr set2)))))))

;; Sets as binary trees

(define (entry:3 tree) (car tree))
(define (left-branch:3 tree) (cadr tree))
(define (right-branch:3 tree) (caddr tree))
(define (make-tree:3 entry left right)
  (list entry left right))

(define (element-of-set?:3 x set)
  (cond ((null? set) false)
        ((= x (entry:3 set)) true)
        ((< x (entry:3 set))
         (element-of-set?:3 x (left-branch:3 set)))
        ((> x (entry:3 set))
         (element-of-set?:3 x (right-branch:3 set)))))

(define (adjoin-set:3 x set)
  (cond ((null? set) (make-tree:3 x '() '()))
        ((= x (entry:3 set)) set)
        ((< x (entry:3 set))
         (make-tree:3 (entry:3 set) 
                    (adjoin-set:3 x (left-branch:3 set))
                    (right-branch:3 set)))
        ((> x (entry:3 set))
         (make-tree:3 (entry:3 set)
                    (left-branch:3 set)
                    (adjoin-set:3 x (right-branch:3 set))))))

;; Exercise 2.63

(define (tree->list-1:3 tree)
  (if (null? tree)
      '()
      (append (tree->list-1:3 (left-branch:3 tree))
              (cons (entry:3 tree)
                    (tree->list-1:3 (right-branch:3 tree))))))

(define (tree->list-2:3 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch:3 tree)
                      (cons (entry:3 tree)
                            (copy-to-list (right-branch:3 tree)
                                          result-list)))))
  (copy-to-list tree '()))

;; Exercise 2.64

(define (list->tree:3 elements)
  (car (partial-tree:3 elements (length elements))))

(define (partial-tree:3 elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree:3 elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree:3 (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree:3 this-entry left-tree right-tree)
                      remaining-elts))))))))

;; Sets and information retrieval


(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key (key (car set-of-records)))
         (car set-of-records))
        (else (lookup given-key (cdr set-of-records)))))

(define (key x) #f) ;; Placeholder
