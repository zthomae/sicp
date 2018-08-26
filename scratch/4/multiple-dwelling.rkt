#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

(define (multiple-dwelling-4)
  (let* ((pick-floor (list 1 2 3 4 5))
         (baker-choices (filter (lambda (baker) (not (= baker 5))) pick-floor)))
    (flatmap
     (lambda (baker)
       (let ((cooper-choices (filter (lambda (cooper) (not (= cooper 1))) pick-floor)))
         (flatmap
          (lambda (cooper)
            (let ((fletcher-choices (filter (lambda (fletcher)
                                              (and (not (= fletcher 5))
                                                   (not (= fletcher 1))
                                                   (not (= (abs (- fletcher cooper)) 1))))
                                            pick-floor)))
              (flatmap
               (lambda (fletcher)
                 (let ((miller-choices (filter (lambda (miller) (> miller cooper)) pick-floor)))
                   (flatmap
                    (lambda (miller)
                      (let ((smith-choices (filter (lambda (smith) (not (= (abs (- smith fletcher)) 1))) pick-floor)))
                        (filter (lambda (result) (distinct? (map cadr result)))
                                (map (lambda (smith)
                                       (list (list 'baker baker)
                                             (list 'cooper cooper)
                                             (list 'fletcher fletcher)
                                             (list 'miller miller)
                                             (list 'smith smith)))
                                     smith-choices))))
                    miller-choices)))
               fletcher-choices)))
          cooper-choices)))
     baker-choices)))