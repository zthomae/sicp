#lang racket/base

(require racket/sandbox)

(provide make-eval)

(define (make-eval)
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string])
    (make-evaluator 'r5rs)))
