;; $Id: sicp-tool.rkt,v 1.11 2011/05/02 09:25:47 neilpair Exp $

;; Note: This was adapted from the PLT 4.1.5 "eopl-tool.ss", with changes
;; specific to SICP and to permitting a version of this package to be in a
;; "PLTCOLLECTS" tree at the same time it is in PLaneT (especially so DrScheme
;; won't complain about duplicate numbers from "get-language-numbers").

#lang mzscheme

(require mzlib/unit
         mzlib/class
         drscheme/tool
         string-constants
         planet/util)

(define (this-package-version/safe)
  (with-handlers ((exn? (lambda (x) #f)))
    (this-package-version)))

(provide tool@)

(define tool@
  (unit
    (import drscheme:tool^)
    (export drscheme:tool-exports^)
    (define language-base%
      (class* object% (drscheme:language:simple-module-based-language<%>)
        (define/public (get-language-numbers)
          ;; TODO: How should we be picking these numbers?
          ;;
          ;; TODO: This checking of PLaneT versions is a kludge to keep
          ;; multiple PLaneT versions from conflicting and making DrScheme
          ;; complain when it starts up.
          `(-500 ,(- -333
                     (cond ((this-package-version/safe)
                            => (lambda (v)
                                 (+ (* 100 (- (list-ref v 2) 1))
                                    (list-ref v 3))))
                           (else 0)))))
        (define/public (get-language-position)
          (list (string-constant teaching-languages)
                (cond ((this-package-version/safe)
                       => (lambda (x)
                            (format "SICP (PLaneT ~S.~S)"
                                    (list-ref x 2)
                                    (list-ref x 3))))
                      (else "SICP"))))
        (define/public (get-module)
          (cond ((this-package-version/safe)
                 => (lambda (x) `(planet "main.rkt" ,x)))
                (else '(lib "main.rkt" "sicp"))))
        (define/public (get-one-line-summary)
          "For use with the SICP textbook")
        (define/public (get-language-url)
          "http://www.neilvandyke.org/racket-sicp/")
        (define/public (get-reader)
          (lambda (src port)
            (let ([v (read-syntax src port)])
              (if (eof-object? v)
                  v
                  (namespace-syntax-introduce v)))))
        (super-instantiate ())))

    (define language%
      (class (drscheme:language:module-based-language->language-mixin
              (drscheme:language:simple-module-based-language->module-based-language-mixin
               language-base%))
        (define/override (use-namespace-require/copy?) #t)
        (define/override (on-execute settings run-in-user-thread)
          (super on-execute settings run-in-user-thread)
          (print-mpair-curly-braces #f)
          (print-pair-curly-braces  #t)
          ;; (run-in-user-thread
          ;;  (lambda ()
          ;;    ((namespace-variable-value 'install-sicp-exception-handler))))
          )
        (super-instantiate ())))

    (define (phase1) (void))
    (define (phase2)
      (drscheme:language-configuration:add-language
       (make-object ((drscheme:language:get-default-mixin) language%))))))
