;load mceval and ambeval
(load "ambeval.scm")

;below defined within ambeval procedure (mce)
;(define-variable! test '(define (multiple-dwelling)
;  (let ((baker (amb 1 2 3 4 5)) (cooper (amb 1 2 3 4 5))
;                                (fletcher (amb 1 2 3 4 5))
;                                (miller (amb 1 2 3 4 5))
;                                (smith (amb 1 2 3 4 5)))
;    (require
;      (distinct? (list baker cooper fletcher miller smith)))
;    (require (not (= baker 5)))
;    (require (not (= cooper 1)))
;    (require (not (= fletcher 5)))
;    (require (not (= fletcher 1)))
;    (require (> miller cooper))
;    (require (not (= (abs (- smith fletcher)) 1)))
;    (require (not (= (abs (- fletcher cooper)) 1)))
;    (list (list 'baker baker) (list 'cooper cooper)
;          (list 'fletcher fletcher) (list 'miller miller)
;  (ambeval '(define (require p) (if (not p) (amb) 'okay))
;          (list 'smith smith)))) the-global-environment)

;SICP 4.41: Solve multiple-dwelling problem without amb
;maybe come back to try after some data structures and algorithms learning

(define (multiple-dwelling1)
  (let ((baker (list 1 2 3 4 5)) (cooper (list 1 2 3 4 5))
                                 (fletcher (list 1 2 3 4 5))
                                 (miller (list 1 2 3 4 5))
                                 (smith (list 1 2 3 4 5)))
    (set! baker (filter (lambda (x) (not (= x 1))) baker))
    (set! cooper (filter (lambda (x) (not (= x 1))) cooper)) 
    (set! fletcher (filter (lambda (x) (not (= x 5)))fletcher))
    (set! fletcher (filter (lambda (x) (not (= x 1))) fletcher))
    (set! miller (filter (lambda (x) (> x (car cooper))) miller))
    (define (loop possible)
      (let ((guess (make-distinct (list baker cooper fletcher miller smith))))
        (cond ((< (cadddr guess) (cadr guess))  (loop ()))
              ((= (abs (- (cadddr (cdr guess)) (cadr guess))) 1) ())
              ((= (abs (- (car guess) (cadr guess))) 1) (new guess))
              (else (list (list 'baker baker) (list 'cooper cooper)
                          (list 'fletcher fletcher) (list 'miller miller))))))
    (loop (list baker cooper fletcher miller smith))))

(define (elim test xlist ylist)
  (map (lambda (x) (test (car xlist) x)) ylist))


;4.42
;below procedure returns (b 3) (e 5) (j 2) (k 1) (m 4)
;requires defining or in terms of (amb) 
(ambeval '(define (liars)
            (let ((betty (amb 1 2 3 4 5)) (ethel (amb 1 2 3 4 5))
                                          (joan (amb 1 2 3 4 5))
                                          (kitty (amb 1 2 3 4 5))
                                          (mary (amb 1 2 3 4 5)))
              (require
                (distinct? (list betty ethel joan kitty mary)))
              (require (or (= kitty 2) (= betty 3)))
              (require (or (= ethel 1) (= joan 2)))
              (require (or (= joan 3) (= ethel 5)))
              (require (or (= kitty 2) (= mary 4)))
              (require (or (= mary 4) (= betty 1)))
              (list (list 'betty betty) (list 'ethel ethel)
                    (list 'joan joan) (list 'kitty kitty) (list 'mary mary))))

           the-global-environment
           (lambda (a b) #t)
           (lambda () #t))

;parsing code from SICP injected into ambeval.scm
(ambeval '(define (or x y) (if x 
                             #t 
                             (if y #t (amb)))) 
         the-global-environment 
         (lambda (a b) #t) 
         (lambda () #t))

(ambeval '(define nouns '(noun student professor cat class)) 
         the-global-environment
         (lambda (a b) #t)
         (lambda () #t))

(ambeval '(define verbs '(verb studies lectures eats sleeps))
         the-global-environment
         (lambda (a b) #t)
         (lambda () #t))

(ambeval '(define articles '(article the a))
         the-global-environment
         (lambda (a b) #t)
         (lambda () #t))

(ambeval '(define *unparsed* '()) 
         the-global-environment
         (lambda (a b) #t)
         (lambda () #t))

(ambeval '(define (parse-noun-phrase)
  (list 'noun-phrase
        (parse-word articles)
        (parse-word nouns)))
           the-global-environment
           (lambda (a b) #t)
           (lambda () #t))

(ambeval '(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*) (cdr word-list)))
  (let ((found-word (car *unparsed*)))
    (set! *unparsed* (cdr *unparsed*))
    (list (car word-list) found-word)))
            the-global-environment
           (lambda (a b) #t)
           (lambda () #t))


(ambeval '(define prepositions '(prep for to in by with))
           the-global-environment
           (lambda (a b) #t)
           (lambda () #t))

(ambeval '(define (parse-prepositional-phrase)
            (list 'prep-phrase
                  (parse-word prepositions)
                  (parse-noun-phrase)))
         the-global-environment
         (lambda (a b) #t)
         (lambda () #t))

(ambeval '(define (parse-verb-phrase)
            (define (maybe-extend verb-phrase)
              (amb verb-phrase
                   (maybe-extend
                     (list 'verb-phrase
                           verb-phrase
                           (parse-prepositional-phrase)))))
            (maybe-extend (parse-word verbs)))
         the-global-environment
         (lambda (a b) #t)
         (lambda () #t))

(ambeval '(define (parse-verb-phrase)
              (amb (parse-word verbs)
                     (list 'verb-phrase
                           (parse-verb-phrase)
                           (parse-prepositional-phrase))))
         the-global-environment
         (lambda (a b) #t)
         (lambda () #t))

(ambeval '(define (parse-sentence)
            (list 'sentence (parse-noun-phrase) (parse-word verbs)))
         the-global-environment
         (lambda (a b) #t)
         (lambda () #t))

(ambeval '(define (parse-simple-noun-phrase)
  (list 'simple-noun-phrase
        (parse-word articles)
        (parse-word nouns)))
           the-global-environment
           (lambda (a b) #t)
           (lambda () #t))

(ambeval '(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
    (amb noun-phrase
         (maybe-extend
          (list 'noun-phrase
                 noun-phrase
                 (parse-prepositional-phrase)))))
  (maybe-extend (parse-simple-noun-phrase)))
           the-global-environment
           (lambda (a b) #t)
           (lambda () #t))

(ambeval '(define (parse-sentence)
            (list 'sentence (parse-noun-phrase) (parse-verb-phrase)))
         the-global-environment
         (lambda (a b) #t)
         (lambda () #t))

(ambeval '(define (parse input)
            (set! *unparsed* input)
            (let ((sent (parse-sentence)))
              (require (null? *unparsed*)) sent))
         the-global-environment
         (lambda (a b) #t)
         (lambda () #t))

;4.45
;The main difference in the five different parse results is the categorization of certain parts of the phrase as
;double categorization of (noun-phrase (simple-noun-phrase x)) and (verb-phrase (verb-phrase (verb-phrase x))) 
;I am not sure how the multiple versions of verb-phrase change the meaning. The difference of noun-phrase vs.
;(noun-phrase (simple-noun-phrase x)) give some nuance of the analysis (that the pattern matched simple-noun-phrase)
;as well as which function was called to parse.

;4.47
;Louis' solution seems to work correctly, it removes the differentiators of repeated (verb-phrase (verb-phrase x))
;as discussed above. I do not believe we are missing any information about the different versions above, but I may be missing something.

;4.48 extended to parse adverbs

(ambeval '(define (parse-simple-verb-phrase)
  (list 'simple-verb-phrase
        (parse-word verbs)
        ))
           the-global-environment
           (lambda (a b) #t)
           (lambda () #t))

(ambeval '(define (parse-adverb-phrase)
            (define (maybe-extend adverb-phrase)
              (amb adverb-phrase
                   (maybe-extend
                     (list 'adverb-phrase
                           adverb-phrase
                           (parse-verb-phrase)))))
            (maybe-extend (parse-simple-adverb-phrase)))
         the-global-environment
         (lambda (a b) #t)
         (lambda () #t))

(ambeval '(define (parse-simple-adverb-phrase)
            (list 'adverb-phrase
                  (parse-word adverbs)
                  ))
         the-global-environment
         (lambda (a b) #t)
         (lambda () #t))

(ambeval '(define (parse-verb-phrase)
            (define (maybe-extend verb-phrase)
              (amb verb-phrase
                   (maybe-extend
                     (list 'verb-phrase
                           verb-phrase
                           (parse-adverb-phrase)
                           ))
                   (maybe-extend
                     (list 'verb-phrase
                           verb-phrase
                           (parse-prepositional-phrase)))
                   ))
            (maybe-extend (parse-simple-verb-phrase)))
         the-global-environment
         (lambda (a b) #t)
         (lambda () #t))

(ambeval '(define (parse-sentence)
            (list 'sentence (parse-noun-phrase) (amb (parse-verb-phrase) (parse-adverb-phrase))))
         the-global-environment
         (lambda (a b) #t)
         (lambda () #t))

;4.50
;change (car choices) to an implemented (random-list choices) in (analyze-amb procedure)
;something like:
(define (random-list choices)
  (let* ((len (length choices))
        (rand (random len)))
    (define (iter choices rand)
        (if (= rand 0)
            (car choices)
            (else (iter (cdr choices (- rand 1))))))
    (iter choices rand)))

;4.52 add if-fail construct to the driver loop to rearrange the arguments appropriately
(define (driver-loop)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ((input (read)))
      (cond ((eq? input 'try-again) (try-again))
            ((eq? (car input) 'if-fail) (ambeval input
                                                the-global-environment
                                                (lambda (val next-alternative)
                                                  (announce-output output-prompt)
                                                  (user-print val)
                                                  (internal-loop next-alternative))
                                                (lambda ()(announce-output ";;; Amb-Eval value:")
                                                                (user-print (cddr input)))))

            (else (begin
                    (newline)
                    (display ";;; Starting a new problem ")
                    (ambeval input
                             the-global-environment
                             ;; ambeval success
                             (lambda (val next-alternative)
                               (announce-output output-prompt)
                               (user-print val)
                               (internal-loop next-alternative))
                             ;; ambeval failure
                             (lambda ()
                               (announce-output
                                 ";;; There are no more values of")
                               (user-print input)
                               (driver-loop))))))))
  (internal-loop
    (lambda ()
      (newline)
      (display ";;; There is no current problem")
      (driver-loop))))
