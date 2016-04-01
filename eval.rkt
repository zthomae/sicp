#lang racket/base

(require racket/sandbox)

(provide make-eval)

(define (make-eval)
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string])
    (call-with-trusted-sandbox-configuration
     (lambda ()
       (make-evaluator 'sicp)))))
