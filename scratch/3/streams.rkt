#lang sicp

(#%require (only racket/base error))
(#%require (only racket/base random))

;; force and delay are already provided

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

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
                            (add-streams (stream-cdr fibs)
                                         fibs))))

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

(define (partial-sums s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (stream-car s) (add-streams (partial-sums s) (stream-cdr s)))))

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

(define S (cons-stream 1 (merge (scale-stream S 2) (merge (scale-stream S 3) (scale-stream S 5)))))

(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

(define (integrate-series s)
  (define (integrate-inner strim index)
    (if (stream-null? strim)
        the-empty-stream
        (cons-stream
         (/ (stream-car strim) index)
         (integrate-inner (stream-cdr strim) (+ index 1)))))
  (integrate-inner s 1))

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define cosine-series
  (cons-stream 1 (integrate-series (scale-stream sine-series -1))))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1)
                  (stream-car s2))
               (add-streams (scale-stream (stream-cdr s2) 
                                          (stream-car s1))
                            (mul-series (stream-cdr s1) 
                                        s2))))

(define (take-stream s n)
  (if (<= n 0)
      the-empty-stream
      (cons-stream (stream-car s)
                   (take-stream (stream-cdr s) (- n 1)))))

(define (drop-stream s n)
  (if (<= n 0)
      s
      (drop-stream (stream-cdr s) (- n 1))))

(define sine-cosine-identity (add-streams (mul-series sine-series sine-series) (mul-series cosine-series cosine-series)))

(define (invert-unit-series s)
  (cons-stream (stream-car s)
               (scale-stream (mul-series (stream-cdr s) (invert-unit-series s)) -1)))

(define (div-series s1 s2)
  (if (= (stream-car s2) 0)
      (error "Cannot divide by 0")
      (mul-series s1 (invert-unit-series s2))))

(define tangent-series (div-series sine-series cosine-series))

(define (average x y )
  (/ (+ x y) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))

(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

(define (stream-limit s tolerance)
  (define (limit-inner last rest)
    (let ((r (stream-car rest)))
      (if (< (abs (- last r)) tolerance)
          r
          (limit-inner r (stream-cdr rest)))))
  (limit-inner (stream-car s) (stream-cdr s)))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(define (ln-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln-summands (+ n 1)))))

(define ln-stream (partial-sums (ln-summands 1)))

(define ln-stream++ (euler-transform ln-stream))

(define ln-stream# (accelerated-sequence euler-transform ln-stream))

(define (stream-values-until-precise s correct tolerance)
  (define (count i last rest)
    (let ((r (stream-car rest)))
      (if (< (abs (- correct (average last r))) tolerance)
          i
          (count (+ i 1) r (stream-cdr rest)))))
  (count 2 (stream-car s) (stream-cdr s)))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (if (or (stream-null? s) (stream-null? t))
      the-empty-stream
      (cons-stream
       (list (stream-car s) (stream-car t))
       (interleave
        (stream-map (lambda (x) (list (stream-car s) x))
                    (stream-cdr t))
        (pairs (stream-cdr s) (stream-cdr t))))))

(define (all-pairs s t)
  (if (or (stream-null? s) (stream-null? t))
      the-empty-stream
      (let ((car-s (stream-car s))
            (car-t (stream-car t))
            (cdr-s (stream-cdr s))
            (cdr-t (stream-cdr t)))
        (cons-stream
         (list car-s car-t)
         (interleave
          (stream-map (lambda (x) (list car-s x))
                      (stream-filter (lambda (y) (not (= y car-s))) cdr-t))
          (interleave
           (stream-map (lambda (x) (list x car-s))
                       cdr-t)
           (pairs cdr-s cdr-t)))))))

(define (triples s t u)
  (if (or (stream-null? s) (stream-null? t) (stream-null? u))
      the-empty-stream
      (cons-stream
       (list (stream-car s) (stream-car t) (stream-car u))
       (interleave
        (stream-map (lambda (p) (cons (stream-car s) p))
                    (pairs (stream-cdr t) (stream-cdr u)))
        (triples (stream-cdr s) (stream-cdr t) (stream-cdr u))))))

(define pythagorean-triples
  (stream-filter
   (lambda (triple)
     (let ((i (car triple))
           (j (cadr triple))
           (k (caddr triple)))
       (= (+ (square i) (square j)) (square k))))
   (triples integers integers integers)))

(define primitive-pythagorean-triples
  (stream-filter
   (lambda (triple)
     (let ((i (car triple))
           (j (cadr triple))
           (k (caddr triple)))
       (= 1 (gcd i (gcd j k)))))
   pythagorean-triples))

(define (merge-weighted weight s t)
  (cond ((stream-null? s) t)
        ((stream-null? t) s)
        (else
         (let* ((a (stream-car s))
                (b (stream-car t))
                (wa (weight a))
                (wb (weight b)))
           (if (< wa wb)
               (cons-stream a (merge-weighted weight (stream-cdr s) t))
               (cons-stream b (merge-weighted weight s (stream-cdr t))))))))

(define (weighted-pairs weight s t)
  (if (or (stream-null? s) (stream-null? t))
      the-empty-stream
      (cons-stream
       (list (stream-car s) (stream-car t))
       (merge-weighted weight
                       (stream-map (lambda (x) (list (stream-car s) x))
                                   (stream-cdr t))
                       (weighted-pairs weight (stream-cdr s) (stream-cdr t))))))

(define (weight-integer-pair p)
  (+ (car p) (cadr p)))

(define (cube x) (* x x x))

(define (weight-cubed p)
  (+ (cube (car p)) (cube (cadr p))))

(define (drop-while f s)
  (cond ((stream-null? s) the-empty-stream)
        ((f (stream-car s)) (drop-while f (stream-cdr s)))
        (else s)))

(define (find-consecutive s)
  (if (stream-null? s)
      the-empty-stream
      (let ((first (stream-car s))
            (rest (stream-cdr s)))
        (if (stream-null? rest)
            the-empty-stream
            (let ((second (stream-car rest)))
              (if (= first second)
                  (cons-stream first
                               (find-consecutive (drop-while (lambda (x) (= x first))
                                                             (stream-cdr rest))))
                  (find-consecutive rest)))))))

(define ramanujan-pairs
  (find-consecutive (stream-map weight-cubed (weighted-pairs weight-cubed integers integers))))

(define (find-three-consecutive-by f g s)
  (if (stream-null? s)
      the-empty-stream
      (let ((first (stream-car s))
            (rest (stream-cdr s)))
        (if (stream-null? rest)
            the-empty-stream
            (let ((second (stream-car rest))
                  (rrest (stream-cdr rest)))
              (if (stream-null? rrest)
                  the-empty-stream
                  (let ((third (stream-car rrest)))
                    (if (= (f first) (f second) (f third))
                        (cons-stream (cons (f first) (map g (list first second third)))
                                     (find-three-consecutive-by
                                      f
                                      g
                                      (drop-while (lambda (x) (= (f x) (f first)))
                                                  (stream-cdr rrest))))
                        (find-three-consecutive-by f g rest)))))))))

(define (weight-squared p)
  (+ (square (car p)) (square (cadr p))))

(define squares-three-ways
  (find-three-consecutive-by
   car
   cadr
   (stream-map (lambda (p) (list (weight-squared p) p))
               (weighted-pairs weight-squared integers integers))))

(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

(define (RC r c dt)
  (lambda (inputs v0)
    (add-streams (scale-stream inputs r)
                 (integral (scale-stream inputs (/ 1 c)) v0 dt))))

(define (smooth stream)
  (if (stream-null? stream)
      the-empty-stream
      (let ((first (stream-car stream))
            (rest (stream-cdr stream)))
        (if (stream-null? rest)
            the-empty-stream
            (let ((second (stream-car rest)))
              (cons-stream (/ (+ first second) 2)
                           (smooth rest)))))))

(define (integral2 delayed-integrand initial-value dt)
  (cons-stream initial-value
               (let ((integrand (force delayed-integrand)))
                 (if (stream-null? integrand)
                     the-empty-stream
                     (integral2 (delay (stream-cdr integrand))
                                (+ (* dt (stream-car integrand))
                                   initial-value)
                                dt)))))

(define (solve-2nd a b dt y0 dy0)
  (define dy (integral ddy dy0 dt))
  (define y (integral dy y0 dt))
  (define ddy (add-streams (scale-stream dy a)
                           (scale-stream y b)))
  y)

(define random-init 0)
(define (rand-update x) (+ x 1))

(define rand
  (let ((x random-init))
    (lambda (m)
      (cond ((eq? m 'generate)
             (begin
               (set! x (rand-update x))
               x))
            ((eq? m 'reset)
             (lambda (new-value)
               (set! x new-value)
               x))
            (else (error "Invalid message -- RAND" m))))))

(define (rand-gen msgs)
  (define (next-value msg last)
    (cond ((eq? msg 'generate) (rand-update last))
          ((and (pair? msg) (eq? (car msg) 'reset)) (cdr msg))
          (else (error "Invalid message -- RAND-GEN" msg))))
  (define s (cons-stream random-init (stream-map next-value msgs s)))
  s)

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (monte-carlo-stream experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo-stream
      (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define (stream-with f) (cons-stream (f) (stream-with f)))

(define (estimate-integral-stream P x1 x2 y1 y2)
  (define (test-point) (P (+ x1 (random (- x2 x1))) (+ y1 (random (- y2 y1)))))
  (monte-carlo-stream (stream-with test-point) 0 0))