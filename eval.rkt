#lang racket/base

(require racket/sandbox)
(require scribble/eval)

(provide make-eval)

(define (make-eval)
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string])
    (make-evaluator 'racket/base)))
