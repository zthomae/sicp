#lang setup/infotab
;; $Id: info.rkt,v 1.20 2012-04-08 07:16:33 user Exp $

(require string-constants)

(define name               "SICP")
(define blurb              '("SICP Support for DrRacket"))
(define homepage           "http://www.neilvandyke.org/racket-sicp/")
(define categories         '(misc))
(define scribblings        '(("sicp.scrbl" ()
                              ;; TODO: This doesn't work.  Is it useful?
                              ;;
                              ;; (language -12)
                              )))
(define can-be-loaded-with 'all)
(define compile-omit-files '("test.rkt"
                             "ch2support.scm"
                             "ch3support.scm"))
(define compile-omit-paths '("code"))

(define required-core-version "5.0")
(define repositories          '("4.x"))

(define tools              '("sicp-tool.rkt"))
(define tool-icons         '("sicp-small.png"))
(define tool-names         '("SICP"))
(define tool-urls          '("http://www.neilvandyke.org/racket-sicp/"))

(define textbook-pls       `((("sicp-small.png" "sicp")
                              "SICP"
                              ,(string-constant teaching-languages)
                              "SICP")))

