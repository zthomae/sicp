(define x 0)

(define (require p)
  (if (not p) (amb)))

(if-fail (begin
           (require (> x 0))
           'ok)
         'failed)

(if-fail (begin
           (require (not (> x 0)))
           'ok)
         'failed)
