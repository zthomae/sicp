(define (require p)
  (if (not p) (amb)))

(define *unparsed* '())

(define nouns '(noun student professor cat class))
(define verbs '(verb studies lectures eats sleeps))
(define articles '(article the a))
(define prepositions '(prep for to in by with))
(define adjectives '(adjective big small red green blue))
(define adverbs '(adverb quickly slowly immediately lazily))
(define conjunctions '(conjunction and but))

(define (amb-select xs)
  (if (null? xs)
      (amb)
      (amb (car xs) (amb-select (cdr xs)))))

(define (traverse-sentence f)
  (list 'sentence
        (traverse-noun-phrase f)
        (traverse-verb-phrase f)))

(define (traverse-word f)
  (lambda (word-list)
    (list (car word-list) (f (cdr word-list)))))

(define parse-word
  (traverse-word
   (lambda (words)
     (require (not (null? *unparsed*)))
     (require (memq (car *unparsed*) words))
     (let ((found-word (car *unparsed*)))
       (set! *unparsed* (cdr *unparsed*))
       found-word))))

(define generate-word
  (traverse-word amb-select))

(define (parse input)
  (set! *unparsed* input)
  (let ((result (traverse-sentence parse-word)))
    (require (null? *unparsed*))
    result))

(define (generate)
  (traverse-sentence generate-word))

(define (traverse-prepositional-phrase f)
  (list 'prep-phrase
        (f prepositions)
        (traverse-noun-phrase f)))

(define (traverse-simple-noun-phrase f)
  (list 'simple-noun-phrase
        (f articles)
        (f nouns)))

(define (traverse-noun-phrase f)
  (define (maybe-add-prep-phrase noun-phrase)
    (amb noun-phrase
         (maybe-add-prep-phrase (list 'noun-phrase
                                      noun-phrase
                                      (traverse-prepositional-phrase f)))))
  (maybe-add-prep-phrase (amb (traverse-simple-noun-phrase f)
                              (traverse-noun-phrase-with-adjective f))))

(define (traverse-noun-phrase-with-adjective f)
  (list 'noun-phrase
        (f articles)
        (traverse-adjective-phrase f)
        (f nouns)))

(define (traverse-verb-phrase f)
  (define (maybe-add-prep-phrase verb-phrase)
    (amb verb-phrase
         (maybe-add-prep-phrase (list 'verb-phrase
                                      verb-phrase
                                      (traverse-prepositional-phrase f)))))
  (maybe-add-prep-phrase (amb (f verbs)
                              (traverse-verb-phrase-with-adverb f))))

(define (traverse-verb-phrase-with-adverb f)
  (list 'verb-phrase
        (f verbs)
        (traverse-adverb-phrase f)))

(define (traverse-with-conjunctions f basis)
  (define (maybe-add-conjunction acc)
    (amb acc
         (maybe-add-conjunction (list 'conjunction
                                      acc
                                      (f conjunctions)
                                      (basis)))))
  (maybe-add-conjunction (basis)))

(define (traverse-adjective-phrase f)
  (traverse-with-conjunctions f (lambda () (f adjectives))))

(define (traverse-adverb-phrase f)
  (traverse-with-conjunctions f (lambda () (f adverbs))))
