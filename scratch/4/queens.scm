(define (require p)
  (if (not p) (amb)))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (any? xs)
  (cond ((null? xs) false)
        ((car xs) true)
        (else (any? (cdr xs)))))

(define (list-ref xs i)
  (if (= i 0)
      (car xs)
      (list-ref (cdr xs) (- i 1))))

(define (or x y)
  (if x
      x
      y))

(define (on-diagonal? i ri j rj)
  ;; assumption: i < j
  (let ((d (- j i)))
    (or
     ;; case 1: diagonal up
     (= rj (+ ri d))
     ;; case 2: diagonal down
     (= ri (+ rj d)))))

(define (map f xs)
  (if (null? xs)
      xs
      (cons (f (car xs)) (map f (cdr xs)))))

(define (safe? k positions)
  (define (conflicts? positions i j)
    (let ((ri (list-ref positions i))
          (rj (list-ref positions j)))
      (or
       (= ri rj)
       (on-diagonal? i ri j rj))))
  (not
   (any?
    (map (lambda (i) (conflicts? positions 0 i))
         (enumerate-interval 1 (- k 1))))))

(define (eight-queens)
  (define (create-position) (amb 1 2 3 4 5 6 7 8))
  (define (iter k positions)
    (if (> k 8)
        positions
        (let* ((next-position (create-position))
               (possible-positions (cons next-position positions)))
          (require (safe? k possible-positions))
          (iter (+ k 1) possible-positions))))
  (iter 1 '()))

(define (amb-select xs)
  (if (null? xs)
      (amb)
      (amb (car xs) (amb-select (cdr xs)))))

(define (n-queens n)
  (define (create-position) (amb-select (enumerate-interval 1 n)))
  (define (iter k positions)
    (if (> k n)
        positions
        (let* ((next-position (create-position))
               (possible-positions (cons next-position positions)))
          (require (safe? k possible-positions))
          (iter (+ k 1) possible-positions))))
  (iter 1 '()))
