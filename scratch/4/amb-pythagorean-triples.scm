(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))

(define (require p)
  (if (not p) (amb)))

(define (an-integer-between i j)
  (require (< i j))
  (amb i (an-integer-between (+ i 1) j)))

(define (pythagorean-triples-sum)
  (let ((sum (an-integer-starting-from 3)))
    (let ((i (an-integer-between 1 sum)))
      (let ((j (an-integer-between i sum)))
        (let ((k (an-integer-between j sum)))
          (require (= sum (+ i j k)))
          (require (= (+ (* i i) (* j j)) (* k k)))
          (list i j k))))))

(define (pythagorean-triples)
  (let ((k (an-integer-starting-from 1)))
    (let ((i (an-integer-between 1 k)))
      (let ((j (an-integer-between i k)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))

(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high))
        (hsq (* high high)))
    (let ((j (an-integer-between i high)))
      (let ((ksq (+ (* i i) (* j j))))
        (require (>= hsq ksq))
        (let ((k (sqrt ksq)))
          (require (integer? k))
          (list i j k))))))