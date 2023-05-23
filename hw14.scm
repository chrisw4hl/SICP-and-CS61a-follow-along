;;;;LAZY EVALUATOR FROM SECTION 4.2 OF
;;;; STRUCTURE AND INTERPRETATION OF COMPUTER PROGRAMS

;;;;Matches code in ch4.scm
;;;; Also includes enlarged primitive-procedures list

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
(load "hw12.scm")


;;  To run without memoization, reload the first version of force-it below


;;;SECTION 4.2.2

;;; Modifying the evaluator
(define (apply1 procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values arguments env))) ; changed
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           (list-of-delayed-args arguments env) ; changed
           (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

(define (meval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (meval (cond->if exp) env))
        ((application? exp)             ; clause from book
         (apply1 (actual-value (operator exp) env)
		   (operands exp)
		   env))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (actual-value exp env)
  (force-it (meval exp env)))

(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps) env)
            (list-of-arg-values (rest-operands exps)
                                env))))

(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
      '()
      (cons (delay-it (first-operand exps) env)
            (list-of-delayed-args (rest-operands exps)
                                  env))))

(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
      (meval (if-consequent exp) env)
      (meval (if-alternative exp) env)))

(define input-prompt ";;; L-Eval input:")
(define output-prompt ";;; L-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output
           (actual-value input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))


;;; Representing thunks

;; non-memoizing version of force-it

(define (force-it obj)
  (if (thunk? obj)
      (actual-value (thunk-exp obj) (thunk-env obj))
      obj))

;; thunks

(define (delay-it exp env)
  (list 'thunk exp env))

(define (thunk? obj)
  (tagged-list? obj 'thunk))

(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))

;; "thunk" that has been forced and is storing its (memoized) value
(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))


;; memoizing version of force-it

(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result (actual-value
                        (thunk-exp obj)
                        (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr obj) result)  ; replace exp with its value
           (set-cdr! (cdr obj) '())     ; forget unneeded env
           result))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))

(define (cons1 x y) (lambda (m) (m x y)))
(define (car1 z) (z (lambda (p q) p)))
(define (cdr1 z) (z (lambda (p q) q)))

;; A longer list of primitives -- suitable for running everything in 4.2
;; Overrides the list in ch4-mceval.scm

(define primitive-procedures
  (list (list 'car car1)
        (list 'cdr cdr1)
        (list 'cons cons1)
        (list 'null? null?)
        (list 'list list)
	(list 'number? number?)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list 'newline newline)
        (list 'display display)
;;      more primitives
        ))

'LAZY-EVALUATOR-LOADED
(define the-global-environment (setup-environment))
;SICP 4.25
;this unless method of defining factorial does not work, since the recursive calls to factorial are always evaluated
;in our applicative order evaluator. The 'exceptional condition' is evaluated even if logically we don't want it to be evaluated.
;Therefore, we never get the return of 1, since it evaluates (factorial 1) * (factorial 0) * (factorial -1) all at the same time.

;SICP 4.26
;implementing the unless procedure as a derived expression or syntactic sugar could be helpful to develop more intuitive absrtactions
;within a program. This is not a useless endeavor, especially if it made code more easily understandable in a 
;particular situation.
;On the other hand, we may wish to have unless available as a procedure, to use in combination or other procedures
;such as map or any other compound procedures

;SICP 4.27
;I believe 4.27 only causes count to reach 2 because of the memoization implemented in the evaluator.
;If you call w again, then check count, it is still 2.

;SICP 4.28
;The forcing of an operand by meval through (actual-value x) would be needed when applying lambdas or compound-procedures
;to arguments. We must take the created thunk which has been used to represent these procedures, and actually evaluate it
;before passing it through to apply

;SICP 4.30
;a. I believe Ben is correct in his explanation of (for-each (lambda (x) (newline) (display x)) (list 1 2 3))
;because newline is a primitive procedure. When the procedure is invoked, both meval and (actual-value) would both
;provide the same answers, as they would simply look up the required variable-value
;b. Cy is correct in his explanation of how mutation is handled by the lazy evaluator. The invocation of set! within
;his proposed examples p1 and p2 are not invoked because of the nature of the lazy evaluator. Therefore,
;(p1 1) should just return 1 and not change x. (p2 1) should be (1 . 2).
;with Cy's proposed changes to the lazy evaluator, the set!s will be called within eval-sequence, and the output will be 
;(p1 1) (1 (1 . 2))
;I was mistaken in my thinking. The orinal lazy evaluator returns (p1 1) (1 2) and (p2 1) 1. This must be because
;the lazy evaluator already evaluates the sequence correctly, but when it is passed to the internal procedure p
;it is evaluated as a compound procedure with a delayed argument, which is never evaluated. The call to e inside
;the p should return the value of the created thunk, not actually call the thunk that was passed.
;Changing the evaluator to compute the actual value of e will change the output.
;c. Changing the implementation of eval-sequence does not affect Ben's example because the call to newline is 
;not delayed because it is in the (proc) portion of (eval-sequence). Only the delayed arguments are affected
;by the lazy evaluation model, which would be the numbers in the list which are self-evaluating.
;d. I believe sequences should be treated as they are in the book's evaluator, as the model of lazy evaluation should
;not be combined with mutation, due to the uncertainty of when in the sequence of evaluation the mutation is called.

;SICP 4.32
;lazier lists can likely better select a certain list element than the show-stream of our stream implementation
;the earlier values of the list will be automatically calculated when needed, and no other implementation details should be needed.

;SICP 4.33
(define (create-cons1-list exp) (if (null? exp) '() (cons1 (car exp) (create-cons1-list (cdr exp)))))
(define (text-of-quotation exp) (if (pair? (cadr exp)) (create-cons1-list (cadr exp)) (cadr exp)))


;SICP 4.36
;not sure, maybe moving on will help my understanding

;4.42
;see hw14_pt2.scm, this is getting too complicated to modify different versions of the evaluator and remember which series of files to load.

