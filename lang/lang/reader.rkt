;; $Id: reader.rkt,v 1.11 2011/05/14 08:18:01 neilpair Exp $

#lang s-exp syntax/module-reader

#:language (cond ((with-handlers ((exn? (lambda (x) #f)))
                    (this-package-version))
                  => (lambda (planet-version)
                       ;; TODO: !!! Is this giving an exact minor version?
                       `(planet "main.ss" ,planet-version)))
                 (else '(lib "main.ss" "sicp")))

#:wrapper1 (lambda (proc)
             (parameterize ((read-accept-infix-dot        #f)
                            (read-case-sensitive          #f)
                            (read-curly-brace-as-paren    #f)
                            (read-square-bracket-as-paren #t))
               (proc)))

(require planet/util)
