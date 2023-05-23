;;;;AMB EVALUATOR FROM SECTION 4.3 OF
;;;; STRUCTURE AND INTERPRETATION OF COMPUTER PROGRAMS

;;;;Matches code in ch4.scm.
;;;; To run the sample programs and exercises, code below also includes
;;;; -- enlarged primitive-procedures list


;;;;This file can be loaded into Scheme as a whole.
;;;;**NOTE**This file loads the metacircular evaluator of
;;;;  sections 4.1.1-4.1.4, since it uses the expression representation,
;;;;  environment representation, etc.
;;;;  You may need to change the (load ...) expression to work in your
;;;;  version of Scheme.

;;;;Then you can initialize and start the evaluator by evaluating
;;;; the expression (mce).


;;**implementation-dependent loading of evaluator file
;;Note: It is loaded first so that the section 4.2 definition
;; of eval overrides the definition from 4.1.1
(load "mceval.scm")



;;;Code from SECTION 4.3.3, modified as needed to run it

(define (amb? exp) (tagged-list? exp 'amb))
(define (amb-choices exp) (cdr exp))

;; analyze from 4.1.6, with clause from 4.3.3 added
;; and also support for Let
(define (analyze exp)
  (cond ((self-evaluating? exp) 
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze (let->combination exp))) ;**
        ((amb? exp) (analyze-amb exp))                ;**
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))

(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))

;;;Simple expressions

(define (analyze-self-evaluating exp)
  (lambda (env succeed fail)
    (succeed exp fail)))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail)
      (succeed qval fail))))

(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env)
             fail)))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env succeed fail)
      (succeed (make-procedure vars bproc env)
               fail))))

;;;Conditionals and sequences

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (pproc env
             ;; success continuation for evaluating the predicate
             ;; to obtain pred-value
             (lambda (pred-value fail2)
               (if (true? pred-value)
                   (cproc env succeed fail2)
                   (aproc env succeed fail2)))
             ;; failure continuation for evaluating the predicate
             fail))))

(define (analyze-sequence exps)
  (define (sequentially a b)
    (lambda (env succeed fail)
      (a env
         ;; success continuation for calling a
         (lambda (a-value fail2)
           (b env succeed fail2))
         ;; failure continuation for calling a
         fail)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))

;;;Definitions and assignments

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env succeed fail)
      (vproc env                        
             (lambda (val fail2)
               (define-variable! var val env)
               (succeed 'ok fail2))
             fail))))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)        ; *1*
               (let ((old-value
                      (lookup-variable-value var env))) 
                 (set-variable-value! var val env)
                 (succeed 'ok
                          (lambda ()    ; *2*
                            (set-variable-value! var
                                                 old-value
                                                 env)
                            (fail2)))))
             fail))))

;;;Procedure applications

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env succeed fail)
      (fproc env
             (lambda (proc fail2)
               (get-args aprocs
                         env
                         (lambda (args fail3)
                           (execute-application
                            proc args succeed fail3))
                         fail2))
             fail))))

(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
      (succeed '() fail)
      ((car aprocs) env
                    ;; success continuation for this aproc
                    (lambda (arg fail2)
                      (get-args (cdr aprocs)
                                env
                                ;; success continuation for recursive
                                ;; call to get-args
                                (lambda (args fail3)
                                  (succeed (cons arg args)
                                           fail3))
                                fail2))
                    fail)))

(define (execute-application proc args succeed fail)
  (cond ((primitive-procedure? proc)
         (succeed (apply-primitive-procedure proc args)
                  fail))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))
          succeed
          fail))
        (else
         (error
          "Unknown procedure type -- EXECUTE-APPLICATION"
          proc))))

;;;amb expressions

(define (analyze-amb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            ((car choices) env
                           succeed
                           (lambda ()
                             (try-next (cdr choices))))))
      (try-next cprocs))))

;;;Driver loop

(define input-prompt ";;; Amb-Eval input:")
(define output-prompt ";;; Amb-Eval value:")

(define (driver-loop)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ((input (read)))
      (if (eq? input 'try-again)
          (try-again)
          (begin
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
                       (driver-loop)))))))
  (internal-loop
   (lambda ()
     (newline)
     (display ";;; There is no current problem")
     (driver-loop))))


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

;;; Support for Let (as noted in footnote 56, p.428)

(define (let? exp) (tagged-list? exp 'let))
(define (let-bindings exp) (cadr exp))
(define (let-body exp) (cddr exp))

(define (let-var binding) (car binding))
(define (let-val binding) (cadr binding))

(define (make-combination operator operands) (cons operator operands))

(define (let->combination exp)
  ;;make-combination defined in earlier exercise
  (let ((bindings (let-bindings exp)))
    (make-combination (make-lambda (map let-var bindings)
                                   (let-body exp))
                      (map let-val bindings))))
                     


;; A longer list of primitives -- suitable for running everything in 4.3
;; Overrides the list in ch4-mceval.scm
;; Has Not to support Require; various stuff for code in text (including
;;  support for Prime?); integer? and sqrt for exercise code;
;;  eq? for ex. solution

;; modified by JDL (5-5-2000)

(define (distinct? items)
  (cond ((null? items) #t)
        ((null? (cdr items)) #t)
        ((member (car items) (cdr items)) #f)
        (else (distinct? (cdr items)))))

(define (require p) (if (not p) amb))


(define (if-fail exp fail)
  (ambeval exp the-global-environment 
           (lambda (val next-alternative)
             (announce-output output-prompt)
             (user-print val))
             fail))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'list list)
        (list 'append append)
        (list 'memq memq)
        (list 'member member)
        (list 'not not)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list '> >)
        (list '>= >=)
        (list '< <)
        (list '<= <=)
        (list 'abs abs)
        (list 'remainder remainder)
        (list 'quotient quotient)
        (list 'number? number?)
        (list 'integer? integer?)
        (list 'boolean? boolean?)
        (list 'string? string?)
        (list 'sqrt sqrt)
        (list 'eq? eq?)
        (list 'distinct? distinct?)
        (list 'require require)
        (list 'equal? equal?)
        (list 'se se)
        (list 'sentence se)
        (list 'first first)
        (list 'butfirst bf)
        (list 'bf bf)
        (list 'display display)
        (list 'if-fail if-fail)
        (list 'ambeval ambeval)
        (list 'even? even?)
        ;;      more primitives
        ))

;;; Added at Berkeley:

(define (mce)
  (set! the-global-environment (setup-environment))
  (ambeval '(define (require p) (if (not p) (amb) 'okay))
           the-global-environment
           (lambda (a b) #t)
           (lambda () #t))

  (ambeval '(define (or x y) (if x 
                               #t 
                               (if y #t (amb)))) 
           the-global-environment 
           (lambda (a b) #t) 
           (lambda () #t))

  (ambeval '(define (multiple-dwelling)
              (let ((baker (amb 1 2 3 4 5)) (cooper (amb 1 2 3 4 5))
                                            (fletcher (amb 1 2 3 4 5))
                                            (miller (amb 1 2 3 4 5))
                                            (smith (amb 1 2 3 4 5)))
                (require
                  (distinct? (list baker cooper fletcher miller smith)))
                (require (not (= baker 5)))
                (require (not (= cooper 1)))
                (require (not (= fletcher 5)))
                (require (not (= fletcher 1)))
                (require (> miller cooper))
                (require (not (= (abs (- smith fletcher)) 1)))
                (require (not (= (abs (- fletcher cooper)) 1)))
                (list (list 'baker baker) (list 'cooper cooper)
                      (list 'fletcher fletcher) (list 'miller miller)
                      (list 'smith smith))))
           the-global-environment
           (lambda (a b) #t)
           (lambda () #t))

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

(ambeval '(define adverbs '(adverb slowly quickly lazily hastily))
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


(ambeval '(define (parse input)
            (set! *unparsed* input)
            (let ((sent (parse-sentence)))
              (require (null? *unparsed*)) sent))
         the-global-environment
         (lambda (a b) #t)
         (lambda () #t))

;(ambeval '(define (if-fail exp fail)
;            (ambeval exp the-global-environment 
;                     (lambda (a b) #t) 
;                     (lambda (val next-alternative)
;                       (announce-output output-prompt)
;                       (user-print fail))))
;         the-global-environment
;         (lambda (a b) #t)
;         (lambda () #t))

(ambeval '(define (an-element-of items)
              (require (not (null? items))) 
              (amb (car items) (an-element-of (cdr items))))
         the-global-environment
         (lambda (a b) #t)
         (lambda () #t))
  (driver-loop))


