(define (require p)
  (if (not p) (amb)))

(define nouns '(noun student professor cat class))

(define verbs '(verb studies lectures eats sleeps))

(define articles '(article the a))

(define (parse-sentence)
  (list 'sentence
        (parse-noun-phrase)
        (parse-verb-phrase)))

(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*) (cdr word-list)))
  (let ((found-word (car *unparsed*)))
    (set! *unparsed* (cdr *unparsed*))
    (list (car word-list) found-word)))

(define *unparsed* '())

(define (parse input)
  (set! *unparsed* input)
  (let ((sent (parse-sentence)))
    (require (null? *unparsed*))
    sent))

(define prepositions '(prep for to in by with))

(define (parse-prepositional-phrase)
  (list 'prep-phrase
        (parse-word prepositions)
        (parse-noun-phrase)))

(define (parse-simple-noun-phrase)
  (list 'simple-noun-phrase
        (parse-word articles)
        (parse-word nouns)))

(define adjectives '(adjective big small red green blue))

(define (parse-noun-phrase-with-adjective--1)
    (list 'noun-phrase
          (parse-word articles)
          (parse-word adjectives)
          (parse-word nouns)))

(define (parse-noun-phrase)
  (define (maybe-add-prep-phrase noun-phrase)
    (amb noun-phrase
         (maybe-add-prep-phrase (list 'noun-phrase
                                      noun-phrase
                                      (parse-prepositional-phrase)))))
  (maybe-add-prep-phrase (amb (parse-simple-noun-phrase)
                              (parse-noun-phrase-with-adjective))))

(define adverbs '(adverb quickly slowly immediately lazily))

(define (parse-verb-phrase-with-adverb--1)
  (list 'verb-phrase
        (parse-word verbs)
        (parse-word adverbs)))

(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase
         (maybe-extend (list 'verb-phrase
                             verb-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (amb (parse-word verbs)
                     (parse-verb-phrase-with-adverb))))

(define conjunctions '(conjunction and but))

(define (parse-adjective-phrase--1)
  (define (maybe-add-conjunction adjective-phrase)
    (amb adjective-phrase
         (maybe-add-conjunction (list 'conjunction
                                      adjective-phrase
                                      (parse-word conjunctions)
                                      (parse-word adjectives)))))
  (maybe-add-conjunction (parse-word adjectives)))

(define (parse-noun-phrase-with-adjective)
    (list 'noun-phrase
          (parse-word articles)
          (parse-adjective-phrase)
          (parse-word nouns)))

(define (parse-adverb-phrase--1)
  (define (maybe-add-conjunction adverb-phrase)
    (amb adverb-phrase
         (maybe-add-conjunction (list 'conjunction
                                      adverb-phrase
                                      (parse-word conjunctions)
                                      (parse-word adverbs)))))
  (maybe-add-conjunction (parse-word adverbs)))

(define (parse-verb-phrase-with-adverb)
  (list 'verb-phrase
        (parse-word verbs)
        (parse-adverb-phrase)))

(define (parse-with-conjunctions parser)
  (define (maybe-add-conjunction acc)
    (amb acc
         (maybe-add-conjunction (list 'conjunction
                                      acc
                                      (parse-word conjunctions)
                                      (parser)))))
  (maybe-add-conjunction (parser)))

(define (parse-adjective-phrase)
  (parse-with-conjunctions (lambda () (parse-word adjectives))))

(define (parse-adverb-phrase)
  (parse-with-conjunctions (lambda () (parse-word adverbs))))

(define (amb-select xs)
  (if (null? xs)
      (amb)
      (amb (car xs) (amb-select (cdr xs)))))

(define (generate-sentence)
  (list 'sentence
        (generate-noun-phrase)
        (generate-verb-phrase)))

(define (generate-word word-list)
  (list (car word-list) (amb-select (cdr word-list))))

(define (generate-prepositional-phrase)
  (list 'prep-phrase
        (generate-word prepositions)
        (generate-noun-phrase)))

(define (generate-simple-noun-phrase)
  (list 'simple-noun-phrase
        (generate-word articles)
        (generate-word nouns)))

(define (generate-noun-phrase)
  (define (maybe-add-prep-phrase noun-phrase)
    (amb noun-phrase
         (maybe-add-prep-phrase (list 'noun-phrase
                                      noun-phrase
                                      (generate-prepositional-phrase)))))
  (maybe-add-prep-phrase (amb (generate-simple-noun-phrase)
                              (generate-noun-phrase-with-adjective))))

(define (generate-noun-phrase-with-adjective)
  (list 'noun-phrase
        (generate-word articles)
        (generate-adjective-phrase)
        (generate-word nouns)))

(define (generate-verb-phrase)
  (define (maybe-add-prep-phrase verb-phrase)
    (amb verb-phrase
         (maybe-add-prep-phrase (list 'verb-phrase
                                      verb-phrase
                                      (generate-prepositional-phrase)))))
  (maybe-add-prep-phrase (amb (generate-word verbs)
                              (generate-verb-phrase-with-adverb))))

(define (generate-verb-phrase-with-adverb)
  (list 'verb-phrase
        (generate-word verbs)
        (generate-adverb-phrase)))

(define (generate-with-conjunctions generator)
  (define (maybe-add-conjunction acc)
    (amb acc
         (maybe-add-conjunction (list 'conjunction
                                      acc
                                      (generate-word conjunctions)
                                      (generator)))))
  (maybe-add-conjunction (generator)))

(define (generate-adjective-phrase)
  (generate-with-conjunctions (lambda () (generate-word adjectives))))

(define (generate-adverb-phrase)
  (generate-with-conjunctions (lambda () (generate-word adverbs))))
