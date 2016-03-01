;; $Id: uninstall.rkt,v 1.5 2011-09-30 11:56:20 user Exp $

#lang racket/base

(require planet/util)

(define sicp-planet-owner "neil")
(define sicp-planet-plt   "sicp.plt")

(define uninstall-sicp
  (let ((nothing-to-uninstall
         (lambda ()
           (printf "No ~A/~A PLaneT versions were found to uninstall.\n"
                   sicp-planet-owner
                   sicp-planet-plt))))
    (lambda ()
      (cond ((assoc sicp-planet-owner (current-cache-contents))
             =>
             (lambda (lst)
               (cond ((assoc sicp-planet-plt (cdr lst))
                      =>
                      (lambda (lst)
                        (for-each
                         (lambda (major-lst)
                           (let ((major (car major-lst)))
                             (for-each
                              (lambda (minor-lst)
                                (for-each
                                 (lambda (minor)
                                   (printf "Uninstalling version ~A.~A...\n"
                                           major
                                           minor)
                                   (remove-pkg sicp-planet-owner
                                               sicp-planet-plt
                                               major
                                               minor))
                                 minor-lst))
                              (cdr major-lst))))
                         (cdr lst))
                        (display "Done uninstalling.\n")))
                     (else (nothing-to-uninstall)))))
            (else (nothing-to-uninstall))))))

(provide uninstall-sicp)
