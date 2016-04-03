#lang racket/base

(require racket/sandbox)

(provide make-eval)

(define (make-eval)
  (define ev (parameterize ([sandbox-output 'string]
                            [sandbox-error-output 'string])
               (call-with-trusted-sandbox-configuration
                (lambda ()
                  (make-evaluator 'sicp)))))
  (ev '(#%require (only racket/base error)))
  (ev '(#%require (only racket/pretty pretty-display)))
  ev)
