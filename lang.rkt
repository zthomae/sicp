;; This file is a modified version of one from the neil/sicp package:
;; http://www.neilvandyke.org/racket/sicp/

#lang r5rs

(#%require (only racket/base
                 current-inexact-milliseconds
                 current-print
                 error
                 flush-output
                 make-parameter
                 random
                 void?)
           (rename racket/base racket:module-begin #%module-begin))

(define-syntax sicp:error
  (syntax-rules ()
    ((_ REASON ARG ...) (error REASON ARG ...))))

(define true #t)

(define false #f)

(define nil '())

(define (identity x) x)

(define (inc x) (+ x 1))

(define (dec x) (- x 1))

(define (square x) (* x x))

(define (cube x) (* x x x))

(define (runtime)
  (inexact->exact (truncate (* 1000 (current-inexact-milliseconds)))))

(define (sicp:random x)
  (if (zero? x)
      (error 'random
             "You called \"(random 0)\".  If you're doing SICP section 1.2.6, don't use 1 for the first argument of \"fast-prime?\".")
      (random x)))

;;; @section Streams

(define-syntax cons-stream
  (syntax-rules ()
    ((_ A B) (cons A (delay B)))))

(define the-empty-stream '())

(define (stream-null? x) (null? x))

;;; @section

(define-syntax sicp-syntax-error
  (syntax-rules ()
    ((_) #f)))

(define-syntax check-expect
  (syntax-rules ()
    ((_ VAL EXPECT)
     (%check-expect-internal 'check-expect
                             equal?
                             (quote VAL)
                             VAL
                             EXPECT))))

(define-syntax check-expect-approx
  (syntax-rules ()
    ((_ VAL EXPECT)
     (%check-expect-internal 'check-expect-approx
                             %approx-equal?
                             (quote VAL)
                             VAL
                             EXPECT))))

(define (%check-expect-internal name check-proc val-syntax val expected)
  (display name)
  (display ": ")
  (write val-syntax)
  (display " \u21D2 ")
  (let ((v VAL))
    (display val)
    (newline)
    (if (check-proc val expected)
        (values)
        (error name
               "Test failed: expected ~S"
               expected))))

(define (%approx-equal? a b)
  (< (abs (- a b)) 1/10000))

(#%provide
 (for-syntax syntax-rules ...)
 (all-from-except r5rs #%module-begin)
 (rename racket:module-begin #%module-begin)
 (rename sicp:error  error)
 (rename sicp:random random)
 check-expect
 check-expect-approx
 cons-stream
 cube
 dec
 false
 identity
 inc
 nil
 runtime
 stream-null?
 square
 the-empty-stream
 true)
