;SICP 4.22: Extend analyze evaluator to support let

(load "hw12.scm")

(define (meval exp env) ((analyze exp) env))

(define (analyze exp)
  (cond ((self-evaluating? exp) (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((let? exp) (analyze-let exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((application? exp) (analyze-application exp))
        (else (error "Unknown expression type: ANALYZE" exp))))

(define (analyze-self-evaluating exp)
  (lambda (env) exp))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env) qval)))

(define (analyze-variable exp)
  (lambda (env) (lookup-variable-value exp env)))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lamda (env)
           (set-variable-value! var (vproc env) env)
           'ok)))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env)
      (define-variable! var (vproc env) env)
      'ok)))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env) (if (true? (pproc env))
                    (cproc env)
                    (aproc env)))))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env) (make-procedure vars bproc env))))

(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))

  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
      first-proc
      (loop (sequentially first-proc (car rest-procs))
            (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs) (error "Empty sequence: ANALYZE"))
    (loop (car procs) (cdr procs))))

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env)
      (execute-application
        (fproc env)
        (map (lambda (aproc) (aproc env))
             aprocs)))))
(define (execute-application proc args)
  (cond ((primitive-procedure? proc)
         (apply-primitive-procedure proc args))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment
            (procedure-parameters proc)
            args
            (procedure-environment proc))))
        (else
          (error "Unknown procedure type: EXECUTE-APPLICATION"
                 proc))))

;4.22 changes: (implement (let? exp) and (let->combination in analyze
(define (let? exp)
  (if (eq? (car exp) 'let)
    #t
    #f))

(define (analyze-let exp)
  (let* ((conv (let->combination exp))
        (vars (lambda-parameters (car conv)))
        (bproc (analyze-sequence (lambda-body (car conv))))
        (args (map analyze (cdr conv))))
 (lambda (env) (execute-application 
                 ((analyze-lambda (car conv)) env)
                 (map (lambda (args) (args env)) args)
                 ))))

(define (let->combination exp)
  (cons (list 'lambda (map car (cadr exp)) (caddr exp)) (map cadr (cadr exp)))) 

;4.23
;I believe Alyssa's procedure described in 4.23 leaves some syntactic analysis to evaluation time.
;On the other hand, the book's version of (analyze-sequence exps) creates the appropriate amount of nested
;lambdas through looping through the first-proc and rest-procs. The nested lambdas created by
;analyze-sequence do not require any further analysis, it can simply be evaluated.
;On the other hand, at evalutaion time, Alyssa's procedure requires looping through the list of procs
;to generate the appropriate lambdas during evaluation time.

;4.24
;It would make sense to run factorial of a large number in both versions of the metacircular evaluator,
;one before analysis evalutator is implented, and once after. Then we can compare the runtimes of the functions
;in both environments. 
;I am able to run (time (meval '(factorial 1000) the-global-environment)) outside of driver loop.
;With analyze implemented, the execution takes 366.67ms. Without the analyze implementation, the execution of
;(time (meval (factorial 1000) the-global-environment)) took 866.68 ms. This shows that there is a very significant
;amount of effort that our evaluator uses just on the syntactic parsing of our procedure. The difference in timing
;shows that the syntactic parsing may be more than half the total evaluations

;CS61A HW13 number 2:
;simple mapreduce without paralellism to try to understand the relavant functions and hide the gorey details
;I was not able to check the answers below
;mapreduce example code taken from CS61A Brian Harvey notes
(define (mapreduce mapper reducer base-case data)
  (groupreduce reducer base-case 
               (sort-into-buckets (map mapper data))))

(define (groupreduce reducer base-case buckets)
  (map (lambda (subset) (make-kv-pair
                          (kv-key (car subset))
                          (accumulate reducer base-case (map kv-value subset))))
  buckets))

(define make-kv-pair cons)
(define kv-key car)
(define kv-value cdr)

(define mt1 '((cs61a-xc . 27) (cs61a-ya . 40) (cs61a-xw . 35)
                (cs61a-xd . 38) (cs61a-yb . 29) (cs61a-xf . 32)))
(define  m t2 '((cs61a-yc . 32) (cs61a-xc . 25) (cs61a-xb . 40)
                        (cs61a-xw . 27) (cs61a-yb . 30) (cs61a-ya . 40)))
(define mt3 '((cs61a-xb . 32) (cs61a-xk . 34) (cs61a-yb . 30)(cs61a-ya . 40) 
                        (cs61a-xc . 28) (cs61a-xf . 33)))

(define sorted-data '(((cs61a-xb . 40) (cs61a-xb . 32))((cs61a-xc . 27) (cs61a-xc . 25) (cs61a-xc . 28))
                      ((cs61a-xd . 38))((cs61a-xf . 32) (cs61a-xf . 33))((cs61a-xk . 34))
                      ((cs61a-xw . 35) (cs61a-xw . 27))
                      ((cs61a-ya . 40) (cs61a-ya . 40) (cs61a-ya . 40))
                      ((cs61a-yb . 29) (cs61a-yb . 30) (cs61a-yb . 30))((cs61a-yc . 32))))

;to generate an inverted index stream, (assuming we were given input data kv pairs (line . (filename file)) 
;I would try:
;(mapreduce (lambda (wd) (make-kv-pair wd (filename file))) append '() (sort-into-buckets file1 file2 file3)) 

;1.b
;(mapreduce (lambda (kv-pair N) (if (< N (length (kv-value kv-pair))) '() kv-pair)) append '() data)
;the above function should filter out kv-pairs with the key < N letters long, while retaining and returning the rest of the list

;2.a
;(mapreduce (lambda (message) (make-kv-pair (caddr message) 1)) + 0 messages)
;2.b
;think i would need to actually run some examples to understand
;3
;I think mapreduce is still the best way to handle these cases of identity mappers, since mapreduce allows parrallelization of the computation.
;If we went to separate map and groupreduce functions, we are missing out on the benefits of parallel computation of large data sets, and running these processes on one machine.

