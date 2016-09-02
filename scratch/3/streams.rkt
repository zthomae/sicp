#lang sicp

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

(define integers-implicitly (cons-stream 1 (add-streams ones integers)))

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
  (cons-stream 1 (integrate-series (scale-stream -1 sine-series))))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))