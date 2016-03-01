#lang r5rs
;; $Id: main.rkt,v 1.22 2011/05/14 08:18:01 neilpair Exp $

(#%require (only racket/base
                 current-inexact-milliseconds
                 current-print
                 error
                 flush-output
                 make-parameter
                 random
                 void?)
           ;; TODO: Is the Racket "#%module-begin" doing anything we don't
           ;; want?
           (rename racket/base racket:module-begin #%module-begin)
;           (all-except (planet soegaard/sicp:2:=1/sicp) cons-stream)
           (only "uninstall.ss" uninstall-sicp))

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

;; Note: This only works with top-level "define" in PLT.
;;
;; (define-syntax sicp-define
;;   (syntax-rules ()
;;     ((_ A B0 B1 ...)
;;      (sicp-define:1 (define A B0 B1 ...) A))))
;;
;; (define-syntax sicp-define:1
;;   (syntax-rules ()
;;     ((_ DEF (X0 X1 ...))
;;      (sicp-define:1 DEF  X0))
;;     ((_ DEF ())
;;      (%sicp-syntax-error "Invalid define form"))
;;     ((_ DEF X0)
;;      (begin DEF (quote X0)))))

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

;;; @section Print Handler

;; (define %current-print-handler-ate-first-void? (make-parameter #f))
;; 
;; (define (%make-print-handler)
;;   (let ((skip-void? #t))
;;     (lambda (val)
;;       (let ((sv? skip-void?))
;;         (set! skip-void? #f)
;;         (or (and sv? (void? val))
;;             (let ((out (current-output-port)))
;;               (display "[PRINT] " out) ; DEBUG
;;               (write val out)
;;               (newline out)
;;               (flush-output out)))))))
;; 
;; (current-print (%make-print-handler))

;;;

(#%provide
 (for-syntax syntax-rules ...)
 (all-from-except r5rs #%module-begin)
 (rename racket:module-begin #%module-begin)
; (all-from (planet soegaard/sicp:2:=1/sicp))
 (all-from "uninstall.ss")
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
