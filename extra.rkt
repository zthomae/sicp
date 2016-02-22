#lang racket/base

(require scribble/base)

(provide (all-defined-out))

(define (repl exp res)
  (verbatim (string-append "> " exp "\n" res)))
